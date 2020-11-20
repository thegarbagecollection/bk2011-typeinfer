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


-- we have to now include a fresh time variable for each lookup
{-
-- OLD - WAS USING TO TEST THAT I HADN'T BROKEN ANYTHING IN THE TRANSFER
builtInAbsType = M.fromList 
        [("succ", emptyTypeScheme $ (FunctionType natType natType, "%X0"))
        ,("pred", emptyTypeScheme  $ (FunctionType natType natType, "%X1"))
        ,("iszero", emptyTypeScheme $ (FunctionType natType boolType, "%X2"))
        ,("cons", emptyTypeScheme $ (FunctionType (TypeVariable "%T0") 
                                                  (FunctionType (TypeConstructor "List" [TypeVariable "%T0"]) 
                                                                (TypeConstructor "List" [TypeVariable "%T0"])) 
                                    , "%X3"))
        ,("head", emptyTypeScheme $ (FunctionType (TypeConstructor "List" [TypeVariable "%T1"]) 
                                                  (TypeVariable "%T1")
                                    , "%X4"))
        ,("tail", emptyTypeScheme $ (FunctionType (TypeConstructor "List" [TypeVariable "%T2"])
                                                  (TypeConstructor "List" [TypeVariable "%T2"])
                                    , "%X5"))
        ,("fix", emptyTypeScheme $ (FunctionType (FunctionType (TypeVariable "%T3") (TypeVariable "%T3"))
                                                 (TypeVariable "%T3")
                                    , "%X6"))
        -- for now we're going to incorporate the B&K modified functions / constructors as their own additional thing, rather than as a replacement
        {-
               fix : (ot->t)->t
               nil : List t
               cons : t --> o List t --> List
               head : List t --> t
               tail : List t --> o List t
        -}
        ,("fixKB", emptyTypeScheme $ (FunctionType (FunctionType (BulletType (TypeVariable "%T4")) (TypeVariable "%T4"))
                                                   (TypeVariable "%T4")
                                     , "%X7"))
                                                   
        ,("nilKB", emptyTypeScheme $ (TypeConstructor "InfList" [TypeVariable "%T5"], "%X8"))
        ,("consKB", emptyTypeScheme $ (FunctionType (TypeVariable "%T6")
                                                    (FunctionType (BulletType (TypeConstructor "InfList" [TypeVariable "%T6"]))
                                                                  (TypeConstructor "InfList" [TypeVariable "%T6"]))
                                      , "%X9"))                           
        ,("headKB", emptyTypeScheme $ (FunctionType (TypeConstructor "InfList" [TypeVariable "%T7"]) 
                                                    (TypeVariable "%T7")
                                      , "%X10"))
        ,("tailKB", emptyTypeScheme $ (FunctionType (TypeConstructor "InfList" [TypeVariable "%T8"])
                                                    (BulletType (TypeConstructor "InfList" [TypeVariable "%T8"]))
                                      , "%X11"))
        ,("mapKB", emptyTypeScheme $ (FunctionType (FunctionType (TypeVariable "%T9") (TypeVariable "%T10")) 
                                                   (FunctionType (TypeConstructor "InfList" [TypeVariable "%T9"])
                                                                 (TypeConstructor "InfList" [TypeVariable "%T10"]))
                                      , "%X12"))
        ,("zipKB", emptyTypeScheme $ (FunctionType (TypeConstructor "InfList" [TypeVariable "%T11"]) 
                                                   (FunctionType (TypeConstructor "InfList" [TypeVariable "%T12"])
                                                                 (TypeConstructor "InfList" [TupleType [TypeVariable "%T11", TypeVariable "%T12"]]))
                                      , "%X13"))
        ,("addTuple", emptyTypeScheme $ (FunctionType (TupleType [natType, natType]) natType, "%X14"))
        ]
-}
        
builtInAbsType = M.fromList 
        [("succ", emptyTypeScheme $ (FunctionType natType natType, "%X0"))
        ,("pred", emptyTypeScheme  $ (FunctionType natType natType, "%X1"))
        ,("iszero", emptyTypeScheme $ (FunctionType natType boolType, "%X2"))
        {-
               fix : (ot->t)->t
               nil : List t
               cons : t --> o List t --> List
               head : List t --> t
               tail : List t --> o List t
        -}
        ,("fix", emptyTypeScheme $ (FunctionType (FunctionType (BulletType (TypeVariable "%T4")) (TypeVariable "%T4"))
                                                   (TypeVariable "%T4")
                                     , "%X7"))
                                                   
        ,("badnil", emptyTypeScheme $ (TypeConstructor "InfList" [TypeVariable "%T5"], "%X8"))
        ,("cons", emptyTypeScheme $ (FunctionType (TypeVariable "%T6")
                                                    (FunctionType (BulletType (TypeConstructor "InfList" [TypeVariable "%T6"]))
                                                                  (TypeConstructor "InfList" [TypeVariable "%T6"]))
                                      , "%X9"))                           
        ,("head", emptyTypeScheme $ (FunctionType (TypeConstructor "InfList" [TypeVariable "%T7"]) 
                                                    (TypeVariable "%T7")
                                      , "%X10"))
        ,("tail", emptyTypeScheme $ (FunctionType (TypeConstructor "InfList" [TypeVariable "%T8"])
                                                    (BulletType (TypeConstructor "InfList" [TypeVariable "%T8"]))
                                      , "%X11"))]
        
        
builtInAbs = M.keys builtInAbsType

builtInKeywords = ["if", "then", "else", "true", "false", "let", "in", "await"]


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
typeConstructors = M.fromList [("List", 1), ("InfList", 1)]


data Token = TokLambda | TokVar String | TokSpace | TokDot | TokOP | TokCP | TokType | TokArrow | 
             TokIf | TokThen | TokElse | TokTrue | TokFalse |
             TokZero | 
             TokOTup | TokCTup | TokComma |
             TokNZDigit Int |
             TokLet | TokIn | TokEq | TokSemicolon |
             TokBullet | TokAwait
            
            
   
data TypeLookup = TypeConstantLookup String
                | TypeConstructorLookup String [TypeLookup]
                | FunctionLookup { flFrom :: TypeLookup, flTo :: TypeLookup }
                | TupleLookup [TypeLookup]      
                | BulletTypeLookup TypeLookup deriving (Show)
             
data Type = TypeConstant String
          | TypeVariable String
          | TypeConstructor String [Type]
          | FunctionType { fFrom :: Type, fTo :: Type }
          | TupleType [Type] 
          | UntypedVariable 
          | BulletType Type deriving (Eq, Ord)
          
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
             | RBullet RawExpr
             | RAwait RawExpr
             
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
          | EBullet Expr
          | EAwait Expr
          
          
    
type TimeVar = String
type TimedType = (Type, TimeVar)
          
data GammaElem = GammaVarType TimedType
               | GammaTypeScheme (S.Set String) TimedType deriving (Show)

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
    show TokBullet = "*"
    show TokAwait = "await "
    
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
    show (RBullet e) = "*" ++ show e
    show (RAwait e) = "await " ++ show e

instance Show Expr where
    show (Application e1 e2) = show e1 ++ " " ++ show e2
    show (Lambda tv e) = "(\\" ++ show tv ++ "." ++ show e ++ ")"
    show (Var (VarName cN iN)) = iN
    show (Lit l) = show l
    show (EIf e1 e2 e3) = "if " ++ show e1 ++ " then " ++ show e2 ++ " else " ++ show e3
    show (ETup eL) = "{" ++ intercalate "," (map show eL) ++ "}"        
    show (ETupAcc e digit size) = show e ++ "." ++ show digit
    show (ELet lb l i) = "let " ++ show lb ++ "=(" ++ show l ++ ") in (" ++ show i ++ ")"
    show (EBullet e) = "*" ++ show e
    show (EAwait e) = "await " ++ show e
    
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
    show (BulletType t) = "*" ++ case t of
                                    FunctionType _ _ -> "(" ++ show t ++ ")"
                                    _ -> show t

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

