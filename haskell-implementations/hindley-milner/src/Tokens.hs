module Tokens where
import Control.Monad.State.Strict
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List (intercalate)
import Control.Applicative
import qualified Data.Set as S

import Debug.Trace

{- 

Taken from the untyped lambda calculus parser and modified
to fit with types. See comments there.

-}


-- Built-in abstractions.
{-
  succ, pred, iszero all work as standard function types
  
  NOTE: all the list built-ins are given unique type variables! if new hardcoded abstractions are added, they must also be given unique type variables!

-}
-- Empty type scheme is a type scheme where no type variables are bound, so all
-- variables in the stored type of the type scheme will be renamed
emptyTypeScheme = GammaTypeScheme S.empty

builtInAbsType = M.fromList 
        [("succ", emptyTypeScheme $ FunctionType natType natType)
        ,("pred", emptyTypeScheme  $ FunctionType natType natType)
        ,("iszero", emptyTypeScheme $ FunctionType natType boolType)
        ,("cons", emptyTypeScheme $ FunctionType (TypeVariable "%T0") 
                                                 (FunctionType (TypeConstructor "List" [TypeVariable "%T0"]) 
                                                               (TypeConstructor "List" [TypeVariable "%T0"])))
        ,("head", emptyTypeScheme $ FunctionType (TypeConstructor "List" [TypeVariable "%T1"]) 
                                                 (TypeVariable "%T1"))
        ,("tail", emptyTypeScheme $ FunctionType (TypeConstructor "List" [TypeVariable "%T2"])
                                                 (TypeConstructor "List" [TypeVariable "%T2"]))
        ,("fix", emptyTypeScheme $ FunctionType (FunctionType (TypeVariable "%T3") (TypeVariable "%T3"))
                                                (TypeVariable "%T3"))
        ,("badnil", emptyTypeScheme $ TypeConstructor "List" [TypeVariable "%T4"])
        ]

builtInAbs = M.keys builtInAbsType

builtInKeywords = ["if", "then", "else", "true", "false", "let", "in"]


-- For type variables: if we look up a type, and a type exists of that name, return that type.
-- if we look up a type, and a type does not exist of that name, but there is a type variable in scope
-- of that name, we use that type variable.
-- if we look up a type, and a type does not exist of that name, and there is no type variable in scope
-- of that name, we create a type variable of that name and use it.
-- 
    
boolType :: Type
boolType = TypeConstant "Bool"

natType :: Type
natType = TypeConstant "Nat"

builtInType = M.fromList [("Bool", boolType), ("Nat", natType)]

typeConstructors :: M.Map String Int
typeConstructors = M.fromList [("List", 1)]


data Token = TokLambda | TokVar String | TokSpace | TokDot | TokOP | TokCP | TokType | TokArrow | 
             TokIf | TokThen | TokElse | TokTrue | TokFalse |
             TokZero | 
             TokOTup | TokCTup | TokComma |
             TokNZDigit Int |
             TokLet | TokIn | TokEq | TokSemicolon
             

            
   
data TypeLookup = TypeConstantLookup String
                | TypeConstructorLookup String [TypeLookup]
                | FunctionLookup { flFrom :: TypeLookup, flTo :: TypeLookup }
                | TupleLookup [TypeLookup]      deriving (Show)
             
data Type = TypeConstant String
          | TypeVariable String
          | TypeConstructor String [Type]
          | FunctionType { fFrom :: Type, fTo :: Type }
          | TupleType [Type] 
          | UntypedVariable deriving (Eq, Ord)
          
data TypeVariableStatus = TypeVariableFree | TypeVariableBound deriving (Eq, Ord, Show)
          
data RawTypedVar = RawTypedVar { rawVarName :: String, rawVarType :: Maybe TypeLookup }

data RLambdaVar = RLambdaVariable RawTypedVar | RLambdaTuple RawTypedTupleVar

data RawTypedTupleVar = RawTypedTupleVar { rawTupleVarNames :: [String], rawTupleVarTypes :: Maybe TypeLookup }

data TypedVar = TypedVar { varName :: VarName, varType :: Type }

data VarName = VarName { codeName :: String, internalName :: String } 

data RawExpr = RApplication RawExpr RawExpr 
             | RLambda RLambdaVar RawExpr 
             | RVar String 
             | RLit Literal 
             | RIf { ifRE :: RawExpr,  thenRE :: RawExpr, elseRE :: RawExpr }
             | RTup [RawExpr]               -- model tuples as lists internally, no need to be efficient here
             | RTupAcc { tupRE :: RawExpr, tupRInd :: Int, tupRSize :: Int }
             | RLet RLetBound RawExpr RawExpr
             
data RLetBound = RLetVar String | RLetTuple [String]
             
data LetBound = LetVar VarName | LetTuple [VarName]

data LambdaVar = LambdaVariable TypedVar | LambdaTuple TypedTupleVar

data TypedTupleVar = TypedTupleVar { tupleVarNames :: [VarName], tupleVarType :: Type }
             
data Literal = BoolLit Bool | NatLitZero

data Expr = Application Expr Expr 
          | Lambda LambdaVar Expr 
          | Var VarName 
          | Lit Literal 
          | EIf { ifE :: Expr, thenE :: Expr, elseE :: Expr }
          | ETup [Expr]
          | ETupAcc { tupE :: Expr, tupInd :: Int, tupSize :: Int }
          | ELet { letBound :: LetBound, letBinding :: Expr, letExpr :: Expr }

data GammaElem = GammaVarType Type 
               | GammaTypeScheme (S.Set String) Type deriving (Show)

instance Show Token where
    show TokLambda = "\\"
    show (TokVar s) = s
    show TokSpace = " "
    show TokDot = "."
    show TokOP = "("
    show TokCP = ")"
    show TokType = ":"
    show TokArrow = "->"
    show TokIf = "if "
    show TokThen = " then "
    show TokElse = " else "
    show TokTrue = "true"
    show TokFalse = "false"
    show TokZero = "0"
    show TokOTup = "{"
    show TokCTup = "}"
    show TokComma = ","
    show (TokNZDigit d) = show d
    show TokLet = "let"
    show TokIn = " in "
    show TokEq = "="
    show TokSemicolon = ";"
               
instance Show RawTypedVar where
    show (RawTypedVar rVN (Just rVT)) = rVN ++ ":" ++ show rVT
    show (RawTypedVar rVN Nothing)    = rVN
    
instance Show TypedVar where
    show (TypedVar (VarName cN iN) vT) = iN ++ ":" ++ show vT
    
instance Show Literal where
    show (BoolLit b) = show b
    show (NatLitZero) = "0"
    

instance Show RawExpr where
    show (RApplication re1 re2) = show re1 ++ " " ++ show re2
    show (RLambda rtv re) = "(\\" ++ show rtv ++ "." ++ show re ++ ")"
    show (RVar s) = s
    show (RLit l) = show l
    show (RIf re1 re2 re3) = "if " ++ show re1 ++ " then " ++ show re2 ++ " else " ++ show re3
    show (RTup reL) = "{" ++ intercalate "," (map show reL) ++ "}"
    show (RTupAcc re digit size) = show re ++ "." ++ show digit
    show (RLet lb rl ri) = "let " ++ show lb ++ "=" ++ show rl ++ " in (" ++ show ri ++ ")"

instance Show Expr where
    show (Application e1 e2) = show e1 ++ " " ++ show e2
    show (Lambda tv e) = "(\\" ++ show tv ++ "." ++ show e ++ ")"
    show (Var (VarName cN iN)) = iN
    show (Lit l) = show l
    show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (ETup eL) = "{" ++ intercalate "," (map show eL) ++ "}"        
    show (ETupAcc e digit size) = show e ++ "." ++ show digit
    show (ELet lb l i) = "let " ++ show lb ++ "=(" ++ show l ++ ") in (" ++ show i ++ ")"
    
instance Show Type where
    show (TypeConstant name) = name
    show (TypeVariable name) = name
    show (TypeConstructor name types) = name ++ "(" ++ intercalate "," (map show types) ++ ")" 
    show (FunctionType f t) = let fromStr = 
                                    case f of
                                        FunctionType f' t' -> "(" ++ show f ++ ")"
                                        _ -> show f
                              in fromStr ++ "->" ++ show t
    show (TupleType types) = "{" ++ intercalate "," (map show types) ++ "}" 
    show UntypedVariable = "#U"

instance Show RLetBound where
    show (RLetVar vN) = vN
    show (RLetTuple vNs) = "{" ++ intercalate "," vNs ++ "}"
             
instance Show LetBound where
    show (LetVar vN) = internalName vN
    show (LetTuple vNs) = "{" ++ intercalate "," (map internalName vNs) ++ "}"    

instance Show LambdaVar where
    show (LambdaVariable tv) = show tv
    show (LambdaTuple ttv) = show ttv
             
instance Show TypedTupleVar where
    show (TypedTupleVar tvn t) = "{" ++ intercalate "," (map internalName tvn) ++ "}:" ++ show t
    
    
instance Show RLambdaVar where
    show (RLambdaVariable v) = show v
    show (RLambdaTuple t) = show t
    
instance Show RawTypedTupleVar where
    show (RawTypedTupleVar rtv (Just rts)) = "{" ++ intercalate "," rtv ++ "}:" ++ show rts
    show (RawTypedTupleVar rtv Nothing) = "{" ++ intercalate "," rtv ++ "}"
    
    
   