{-

Parser generator for typed lambda calculus with 
multiple-parameter lambdas and parens.

Based on the previous untyped version.

Types are included with an optional ":T" after the
variable name.

-}

{
module Parser where
import Tokens
import Errors
import Data.List (nub, intersect, (\\), union, intersperse)

}

%name parseRawExpr
%tokentype { Token }
%monad { (Either Error) }
%error { parseError }

%token
    '.'     { TokDot }
    lambda  { TokLambda }
    ' '     { TokSpace }
    var     { TokVar $$ }
    '('     { TokOP }
    ')'     { TokCP }
    ':'     { TokType }
    arr     { TokArrow }
    if      { TokIf }
    then    { TokThen }
    else    { TokElse }
    true    { TokTrue }
    false   { TokFalse }
    '0'     { TokZero }
    '{'     { TokOTup }
    '}'     { TokCTup }
    ','     { TokComma }
    nzdigit { TokNZDigit $$ }
    let     { TokLet }
    in      { TokIn }
    '='     { TokEq }
    ';'     { TokSemicolon }
    
%right '.' arr 
%right if then else let in 
%left ' '
%right '='


        
%%

{-

Lambda-calculus grammar augmented with 
- parens, 
- multi-parameter lambdas, auto-curried

expr := '(' expr ')'
      | expr ' ' expr               -- left-associative
      | '\' var '.' expr
      | var

-}


Expr    : '(' Expr ')'                { $2 }
        | Expr ' ' Expr               { RApplication $1 $3 }
        | lambda VarListT '.' Expr    { % createNestedLambda $4 $2 }
        | var                         { RVar $1 }
        | Literal                     { RLit $1 }
        | if ' ' Expr ' ' then ' ' Expr ' ' else ' ' Expr { RIf $3 $7 $11 }
        | '{' TupleContents '}'           { RTup $2 }       -- note: this forbids the zero-tuple {}
        | TupleAccessor               { $1 }
        | let ' ' LetContents ' ' in ' ' Expr         { % createNestedLet $7 $3 }
        
TupleAccessor : Expr '.' nzdigit            { RTupAcc $1 $3 }
              
LetContents : LetBound '=' Expr                 { [($1,$3)] }
            | LetBound '=' Expr ';' LetContents { ($1,$3) : $5 }

LetBound : var                       { RLetVar $1 }
         | '{' CommaSepVarList '}'   { RLetTuple $2 }
            
CommaSepVarList  : var               { [$1] }
                 | var ',' CommaSepVarList { $1 : $3 }
       
TupleContents : Expr ',' TupleContents    { $1 : $3 }
              | Expr                      { [$1] }
        
Literal : true                        { RBoolLit True }
        | false                       { RBoolLit False }
        | '0'                         { RNatLitZero }
        
VarListT : VarT                     { [$1] }
         | VarT ' ' VarListT        { $1 : $3 }

VarT : var                          { RLambdaVariable $ RawTypedVar $1 Nothing }
     | var ':' Type                 { RLambdaVariable $ RawTypedVar $1 (Just $3) }
     | '{' CommaSepVarList '}'           { RLambdaTuple $ RawTypedTupleVar $2 Nothing }
     | '{' CommaSepVarList '}' ':' Type  { RLambdaTuple $ RawTypedTupleVar $2 (Just $5) }


Type : '(' Type ')'                 { $2 }
     | Type arr Type                { FunctionLookup $1 $3 }        
     | TypeConstr                   { $1 }
     | '{' TypeList '}'             { TupleLookup $2 }
      
TypeConstr : var                    { TypeConstantLookup $1 }
           | var '(' TypeList ')'   { TypeConstructorLookup $1 $3 }
      
TypeList : Type                     { [$1] }
         | Type ',' TypeList        { $1 : $3 }


{ 

parseError :: [Token] -> Either Error a
parseError tok = Left $ ParseFailure $ "PARSING FAILURE: Parse error with remaining string: " ++ concat (map show tok)

-- For multi-variable abstractions
-- Given an expression expr and a list of variable names vars=[v1:T1,...,vn:Tn],
-- create the lambda abstraction (\v1:T1. ... .\vn:Tn.expr)
createNestedLambda :: RawExpr -> [RLambdaVar] -> Either Error RawExpr
createNestedLambda expr vars = fmap (varList expr) $ toRawTypes vars
    

-- varList e vs = foldr (\vName e' -> Lambda vName e') e vs
varList :: RawExpr -> [RLambdaVar] -> RawExpr
varList = foldr RLambda

-- All of this is inefficient due to horrible list manipulations.
-- Addendum: the lists are small enough that it shouldn't matter!
toRawTypes :: [RLambdaVar] -> Either Error [RLambdaVar]
toRawTypes ss = case (nonUnique == [], keywordClash == []) of
                    (True, True) -> Right $ ss
                    (True, False) -> Left $ ParseFailure errorKeyword
                    (False, True) -> Left $ ParseFailure errorUniqueness
                    (False, False) -> Left $ ParseFailure $ errorUniqueness ++ "\n" ++ errorKeyword
            where names = ss >>= (\x -> case x of
                                            RLambdaVariable (RawTypedVar n _) -> [n]
                                            RLambdaTuple (RawTypedTupleVar ns _) -> ns)
                  nonUnique = nub (names \\ nub names)
                  keywordClash = nub $ (names `intersect` builtInAbs) `union` (names `intersect` builtInKeywords)
                  listSep = concat . intersperse ", "
                  errorUniqueness = "PARSING FAILURE: Non-unique variable names in multi-variable abstraction: " ++ listSep nonUnique
                  errorKeyword = "PARSING FAILURE: variable name clash with built-in keywords or abstractions: " ++ listSep keywordClash


                  
-- Same kind of stuff, but for multi-binding let-expressions
-- This definition is fine except for no error handling; we can also simplify
-- createNestedLet inExpr letBindings = Right $ foldr (\(name, boundExpr) acc -> RLet name boundExpr acc) inExpr letBindings
createNestedLet :: RawExpr -> [(RLetBound, RawExpr)] -> Either Error RawExpr
createNestedLet inExpr letBindings = fmap (foldr (uncurry RLet) inExpr) $ checkLet letBindings
                  
                  
checkLet :: [(RLetBound, RawExpr)] -> Either Error [(RLetBound, RawExpr)]
checkLet ss = case (nonUnique == [], keywordClash == []) of
                    (True, True) -> Right $ ss
                    (True, False) -> Left $ ParseFailure errorKeyword
                    (False, True) -> Left $ ParseFailure errorUniqueness
                    (False, False) -> Left $ ParseFailure $ errorUniqueness ++ "\n" ++ errorKeyword
            where names = ss >>= (\(bound,_) -> case bound of
                                                    RLetVar s -> [s]
                                                    RLetTuple ss -> ss)
                  nonUnique = nub (names \\ nub names)
                  keywordClash = nub $ (names `intersect` builtInAbs) `union` (names `intersect` builtInKeywords)
                  listSep = concat . intersperse ", "
                  errorUniqueness = "PARSING FAILURE: Non-unique variable names in let binding: " ++ listSep nonUnique
                  errorKeyword = "PARSING FAILURE: let-binding variable name clash with built-in keywords or abstractions: " ++ listSep keywordClash

                  
                 


}