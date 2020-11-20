module TypeInference where


import Tokens
import Renaming
import Errors
import Data.Either
import Data.Map.Strict (empty, insert, (!?), Map, fromList, (!))
import qualified Data.Set as S
import Control.Monad (join)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Debug.Trace


-- Use Left for error message, Right for valid type
-- This means failing on the first type error found.
type StateError s a = StateT s (Either Error) a
type Sigma = Map String Type
type TypeVarScope = S.Set String

-- Gamma, from the specification
data TypeEnv = TE { varTypes :: Map String Type, boundTVs :: TypeVarScope }

runInfer :: Expr -> Either Error Type
runInfer e = inferType e (TE builtInAbsType S.empty)


runInfer2 :: Either Error Expr -> Either Error Type
runInfer2 e = e >>= runInfer


inferTypeFromGamma :: Expr -> Map String Type -> Either Error Type
inferTypeFromGamma e gammaInit = inferType e (TE gammaInit S.empty)


match' :: TypeVarScope -> Type -> Type -> Either Error Sigma
match' boundTVs t1 t2 = 
    case runStateT (match boundTVs t1 t2) empty of
        Right (_, sigma) -> Right sigma
        Left msg -> Left msg
                

-- match should modify the input state to contain an assignment of types to type variables
match :: TypeVarScope -> Type -> Type -> StateError Sigma ()

match _ (TypeConstant c) (TypeConstant c2) = 
    if c == c2
    then return ()
    else lift $ Left $ TypeCheckFailure $ "Type match failed: expected type constant " ++ c ++ ", but given " ++ c2

-- We know by passing the preprocessing that if names are the same then the type lists must have equal length
match boundTVs (TypeConstructor name types) (TypeConstructor name2 types2) = 
    if name /= name2 
    then lift $ Left $ TypeCheckFailure $ "Type match failed: expected type constructor " ++ name ++ ", but given " ++ name2
    else mapM_ (uncurry $ match boundTVs) (zip types types2) 
    
match boundTVs (FunctionType f t) (FunctionType f2 t2) = mapM_ (uncurry $ match boundTVs) [(f, f2), (t, t2)]

-- Tuple lengths need checking
match boundTVs t1@(TupleType types1) t2@(TupleType types2) =
    if length types1 /= length types2
    then lift $ Left $ TypeCheckFailure $ "Type match failed: n-tuple match incorrect of " ++ show t1 ++ " against " ++ show t2
    else mapM_ (uncurry $ match boundTVs) (zip types1 types2)
    

-- And the tricky one
match boundTVs tv@(TypeVariable t) tv2 =
    case isTypeVariableBound t boundTVs of
        TypeVariableBound -> case tv2 of
                (TypeVariable t2) -> case isTypeVariableBound t2 boundTVs of
                                TypeVariableBound -> if t == t2 
                                                     then return () 
                                                     else lift $ Left $ TypeCheckFailure $ "Type match failed: type variable " ++ show tv ++ " was bound, and expected a bound type variable, but was given the different bound type variable " ++ show tv2
                                TypeVariableFree -> lift $ Left $ TypeCheckFailure $ "Type match failed: type variable " ++ show tv ++ " was bound, and expected a bound type variable, but was given the unbound type variable " ++ show tv2
                                
                _ -> lift $ Left $ TypeCheckFailure $ "Type match failed: type variable " ++ show tv ++ " was bound, and expected a bound type variable, but was given " ++ show tv2
        TypeVariableFree -> do
            sigma <- get
            case sigma !? t of
                Just u ->  if u == tv2
                           then return ()
                           else lift $ Left $ TypeCheckFailure $ "Type match failed: type variable " ++ t ++ " expected " ++ show u ++ ", but given " ++ show tv2
                Nothing -> put (insert t tv2 sigma)
            

  
match _ (TypeConstant c) t2 =
    lift $ Left $ TypeCheckFailure $ "Type match failed: expected type constant " ++ c ++ ", but given " ++ show t2

match _ (TypeConstructor name _) t2 = 
    lift $ Left $ TypeCheckFailure $ "Type match failed: expected type constructor " ++ name ++ ", but given " ++ show t2

match _ f@(FunctionType _ _) t2 = 
    lift $ Left $ TypeCheckFailure $ "Type match failed: expected function type " ++ show f ++ ", but given " ++ show t2

match _ t@(TupleType _) t2 = 
    lift $ Left $ TypeCheckFailure $ "Type match failed: expected tuple type " ++ show t ++ ", but given " ++ show t2

 

    
substitute :: Sigma -> TypeVarScope -> Type -> Type

substitute _ _ tc@(TypeConstant _) = tc

-- Bound type variables don't get looked up; only free type variables do
substitute sigma boundTVs tv@(TypeVariable name)  = 
    case isTypeVariableBound name boundTVs of
        TypeVariableBound -> tv
        TypeVariableFree -> sigma ! name
    
substitute sigma boundTVs (TypeConstructor n ts) = TypeConstructor n (map (substitute sigma boundTVs) ts)
    
substitute sigma boundTVs (FunctionType f t) = FunctionType (substitute sigma boundTVs f) (substitute sigma boundTVs t)

substitute sigma boundTVs (TupleType ts) = TupleType (map (substitute sigma boundTVs) ts)
    

    
inferType :: Expr -> TypeEnv -> Either Error Type

-- Replacement checking rule for type substitutions
-- G |- t1 : T11->T12   G |- t2 : T11'
-- match(T11, T11') = sigma
-- -----------------------------------
-- G |- t1 t2 : sigma(T12)
inferType (Application f expr) gamma = do
    tF <- inferType f gamma
    t11' <- inferType expr gamma
    case tF of
        FunctionType t11 t12 -> 
            case match' (boundTVs gamma) t11 t11' of
                Right sigma -> return $ substitute sigma (boundTVs gamma) t12 
                Left err -> Left err
        _ -> Left $ TypeCheckFailure $ "Type-checking failed: non-abstraction of type " ++ show tF ++ " applied to argument of type " ++ show t11'



-- G,x:T1 |- t2:T2
-- ----------------------
-- G |- \x:T1.t2 : T1->T2
inferType (Lambda (LambdaVariable typedVar) expr) gamma = do
--    we don't need to do this here - any parsed type is considered valid
--    t1 <- checkDeclaredType typedVar
    let varTypes' = insert (internalName . varName $ typedVar) (varType typedVar) $ varTypes gamma
        boundTVs' = bindTypeVariables (varType typedVar) (boundTVs gamma)
    t2 <- inferType expr (TE varTypes' boundTVs')
    return $ FunctionType (varType typedVar) t2


    
inferType l@(Lambda (LambdaTuple (TypedTupleVar vNs vT)) expr) gamma = 
    case vT of
        TupleType ts | length vNs == length ts -> do
            let varTypes' = foldr (uncurry insert) (varTypes gamma) (zip (map internalName vNs) ts)
                boundTVs' = bindTypeVariables vT (boundTVs gamma)
            t2 <- inferType expr (TE varTypes' boundTVs')
            return $ FunctionType vT t2
        _ -> Left $ TypeCheckFailure $ "Type-checking failed: got a tuple pattern-matching abstraction " ++ show l ++ " where the type " ++ show vT ++ " was not a tuple or whose size did not match the required size."

    
-- x:T in G
-- --------
-- G |- x:T
inferType (Var varName) gamma = 
    case varTypes gamma !? internalName varName of
        Just t -> return t
        Nothing -> Left $ TypeCheckFailure $ "Type-check failed: could not find variable " ++ codeName varName ++ " in type environment."

        
-- G |- true : Bool
-- G |- false : Bool
inferType (Lit (BoolLit _)) gamma = return boolType

-- G |- 0 : Nat
inferType (Lit NatLitZero) gamma = return natType

-- t1:Bool, t2:T, t3:T
-- --------------------
-- if t1 then t2 else t3 : T
inferType (EIf ifExpr thenExpr elseExpr) gamma = do
    t1 <- inferType ifExpr gamma
    t2 <- inferType thenExpr gamma
    t3 <- inferType elseExpr gamma
    if t1 == boolType && typeEqualsRename (boundTVs gamma) t2 t3 -- typeEquals t2 t3
    then return t2
    else Left $ TypeCheckFailure $ "Type-check failed: if " ++ show t1 ++ " then " ++ show t2 ++ " else " ++ show t3

 
    
    
    
  
-- for all i  G |- ti : Ti
-- --------------------------------------------
-- G |- {ti for i in 1..n} : {Ti for i in 1..n}
inferType (ETup exprList) gamma = do
    tL <- mapM (flip inferType gamma) exprList
    return $ TupleType tL


-- G |- t1 : {Ti for i in 1..n}
-- ----------------------------
-- G |- t1.j : Tj
inferType (ETupAcc expr i) gamma = do
    t1 <- inferType expr gamma
    case t1 of
        TupleType lT -> let i' = i - 1         -- tuples are 1-indexed, unlike lists
                        in if i' >= length lT  -- don't need to check -ve, since '-' not accepted by parser; also, 0 on its own is the only Nat literal so won't be parsed as a tuple accessor.
                           then Left $ TypeCheckFailure $ "Type-check failed: tuple index of " ++ show i ++ " too large for tuple of length " ++ show (length lT) ++ " (type " ++ show (TupleType lT) ++ ")"
                           else return $ lT !! i'
        _ -> Left $ TypeCheckFailure $ "Type-check failed: tuple accessor ." ++ show i ++ " given " ++ show t1




        
-- and let bindings too! type checking for these is as in Pierce (2002)
-- unless problems with parametric polymorphism show up.
-- G |- t1:T1     G, x:T1 |- t2:T2
-- -------------------------------
-- G |- let x=t1 in t2    : T2
inferType (ELet (LetVar vName) binding inExpr) gamma = do
    t1 <- inferType binding gamma
    let varTypes' = insert (internalName vName) t1 $ varTypes gamma
    t2 <- inferType inExpr (TE varTypes' $ boundTVs gamma)
    return t2

        
        
inferType (ELet (LetTuple vNames) binding inExpr) gamma = do
    t1 <- inferType binding gamma
    case t1 of
        TupleType ts | length vNames == length ts -> do
            let varTypes' = foldr (uncurry insert) (varTypes gamma) (zip (map internalName vNames) ts)
            t2 <- inferType inExpr (TE varTypes' $ boundTVs gamma)
            return t2
        _ -> Left $ TypeCheckFailure $ "Type-check failed: tuple pattern-match in let binding failed, given variables " ++ show (LetTuple vNames) ++ " but the expression had type " ++ show t1
        
        
-- We consider two types 'equal' if their structures match recursively,
-- and their type variables are identical up to renaming, ignoring bound/free
-- variable state (think this is ok here).
typeEqualsRename :: TypeVarScope -> Type -> Type -> Bool
typeEqualsRename boundTVs t1 t2 = evalState (typeEqualsRename' boundTVs t1 t2) (empty, empty)

-- We need an equivalence of types, not an implication -
-- T1 <==> T2
-- which means a dual map

typeEqualsRename' :: TypeVarScope -> Type -> Type -> State (Sigma, Sigma) Bool

typeEqualsRename' boundTVs (TypeConstant c1) (TypeConstant c2) = return $ c1 == c2

typeEqualsRename' boundTVs (FunctionType f1 t1) (FunctionType f2 t2) = do
    fEq <- typeEqualsRename' boundTVs f1 f2
    tEq <- typeEqualsRename' boundTVs t1 t2
    return $ fEq && tEq
    
typeEqualsRename' boundTVs (TupleType ts1) (TupleType ts2) = do
    eqs <- mapM (uncurry $ typeEqualsRename' boundTVs) (zip ts1 ts2)
    return $ length ts1 == length ts2 && and eqs

typeEqualsRename' boundTVs (TypeConstructor n1 ts1) (TypeConstructor n2 ts2) = do
    eqs <- mapM (uncurry $ typeEqualsRename' boundTVs) (zip ts1 ts2)
    return $ n1 == n2 && and eqs


typeEqualsRename' boundTVs tv1@(TypeVariable name1) tv2@(TypeVariable name2) = do
    -- lookupTable1 maps the name of type variable 1 to a type variable 2 to which it's bound
    -- vice versa for lookupTable2
    case (isTypeVariableBound name1 boundTVs, isTypeVariableBound name2 boundTVs) of
        (TypeVariableBound, TypeVariableBound) -> return $ name1 == name2
        
        (TypeVariableFree, TypeVariableFree) -> do
            (lookupTable1, lookupTable2) <- get
            case (lookupTable1 !? name1, lookupTable2 !? name2) of
                (Just _, Nothing) -> return False
                (Nothing, Just _) -> return False
                
                -- We can ignore the bound state of type variables here due to the below
                (Just (TypeVariable name2'), Just (TypeVariable name1')) -> 
                    return $ name2 == name2' && name1 == name1'
                    
                (Nothing, Nothing) -> do
                    -- note that entries into the lookup tables, by definition, are
                    -- not bound type variables
                    put (insert name1 tv2 lookupTable1, insert name2 tv1 lookupTable2)
                    return True
                    
        (_, _) -> return False
    

typeEqualsRename' _ _ _ = return False


isTypeVariableBound :: String -> S.Set String -> TypeVariableStatus
isTypeVariableBound name bindings = if name `S.member` bindings
                                    then TypeVariableBound
                                    else TypeVariableFree

                                    
bindTypeVariables :: Type -> TypeVarScope -> TypeVarScope

bindTypeVariables (TypeConstant _) boundTVs = boundTVs 

bindTypeVariables (TypeVariable name) boundTVs = S.insert name boundTVs

bindTypeVariables (TypeConstructor _ ts) boundTVs = foldr bindTypeVariables boundTVs ts

bindTypeVariables (TupleType ts) boundTVs = foldr bindTypeVariables boundTVs ts

bindTypeVariables (FunctionType f t) boundTVs = foldr bindTypeVariables boundTVs [f, t]