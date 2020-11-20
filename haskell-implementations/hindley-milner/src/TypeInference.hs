module TypeInference where


import Tokens
import Renaming
import Errors
import Data.Either
import Data.Map.Strict (empty, insert, (!?), Map, fromList, (!), fromSet)
import qualified Data.Set as S
import Control.Monad (join, replicateM)

import Data.List (intercalate)

import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Class

import Debug.Trace


-- Use Left for error message, Right for valid type
-- This means failing on the first type error found.
type StateError s a = StateT s (Either Error) a

type Sigma = Type -> Type
type TypeVarScope = S.Set String

type TypeVariables = S.Set String

-- Gamma, from the specification
-- I'm not sure if the bound/free status of type variables is of any use for recon/unif
-- Keep it around anyway
data TypeEnv = TE { varTypes :: Map String GammaElem, boundTVs :: TypeVarScope }

runInference :: Either Error Expr -> Either Error Type
runInference e = e >>= inferType

-- A constraint (T1, T2) can be interpreted as T1=T2
type Constraints = S.Set Constraint

data Constraint = Constraint Type Type deriving (Eq, Ord)

instance Show Constraint where
    show (Constraint t1 t2) = show t1 ++ " = " ++ show t2


-- We use the same fresh type variable generator as in the previous version - just a stream
-- of ?T1,...,?Tk,... where  1.. is the infinite stream of integers
data FreshTV = FreshTV { getFresh :: String, getNextFreshTV :: FreshTV }

fresh :: Integer -> FreshTV
fresh n = FreshTV ("?T" ++ show n) (fresh $ n + 1)



data TypeScheme = TypeScheme { boundInTypeScheme :: TypeVarScope, typeInTypeScheme :: Type }




        
        
-- TODO: this is basically the same as rename; we can redefine rename
-- as this, but ensuring that the type scheme contains no bound TVs
extractType :: TypeScheme -> StateError (Constraints, FreshTV) Type 

extractType (TypeScheme boundTVs typeToRename) = do
    (constraints, freshTV) <- get
    let startingBindings = fromSet (\name -> TypeVariable name) boundTVs
        (renamed, (_, freshTV')) = runState (extractType' typeToRename) (startingBindings, freshTV)
    put (constraints, freshTV')
    return renamed


extractType' :: Type -> State (Map String Type, FreshTV) Type
    
extractType' tc@(TypeConstant _) = return tc

extractType' (FunctionType f t) = do
    f' <- extractType' f
    t' <- extractType' t
    return $ FunctionType f' t'
        
extractType' (TypeConstructor name ts) = do
    ts' <- mapM extractType' ts
    return $ TypeConstructor name ts'
    
extractType' (TupleType ts) = do
    ts' <- mapM extractType' ts
    return $ TupleType ts'
    

extractType' tv@(TypeVariable n) = do
    (bindings, _) <- get
    case bindings !? n of
        Just t -> return t
        Nothing -> do
            tv' <- nextFresh
            updateBindings n tv'
            return tv'
            
updateBindings :: String -> Type -> State (Map String Type, FreshTV) ()
updateBindings name t = modify' f
    where f (bindings, freshTV) = (insert name t bindings, freshTV)
    
    
    

constraintsTemp :: Either Error Expr -> Either Error (Type, Constraints)
constraintsTemp e = do
    expr <- e
    (resultType, (constraints, _)) <- runStateT (constrain expr (TE builtInAbsType S.empty)) (S.empty, fresh 1)
    return $ (resultType, constraints)
    
    
constraintsTemp2 :: Map String GammaElem -> Either Error Expr -> Either Error (Type, Constraints)
constraintsTemp2 gamma e = do
    expr <- e
    (resultType, (constraints, _)) <- runStateT (constrain expr (TE gamma S.empty)) (S.empty, fresh 1)
    return $ (resultType, constraints)
    
    
    
inferType :: Expr -> Either Error Type
inferType expr = do
        (resultType, (constraints, _)) <- runStateT (constrain expr (TE builtInAbsType S.empty)) (S.empty, fresh 1)
        sigma <- unify constraints
        return $ substitute resultType sigma                                       
                                

-- REPL thing!!                    ************************************************************************************************************************************************************************************************************************************************************************
inferTypeFromGamma :: Expr -> Map String GammaElem -> Either Error Type
inferTypeFromGamma expr gammaInit = do
        (resultType, (constraints, _)) <- runStateT (constrain expr (TE gammaInit S.empty))  (S.empty, fresh 1)
        sigma <- unify constraints
        return $ substitute resultType sigma

                                
                                    
unify :: Constraints -> Either Error Sigma
unify constraints = 
    let constraintList = S.toList constraints
    in unify' constraintList

-- Bit awkward - to get a single element out, we effectively need
-- to pattern match on a list, we can't use some kind of set constructor
-- or pick an arbitrary element. But now we can't guarantee that a 
-- substitution created won't be a duplicate in some way. Don't THINK
-- this is a problem - if unification would succeed in the absence of 
-- duplicates, then the duplicates become case S=T and are removed.
-- If not, unification won't succeed anyway.

-- We're going to break this down explicitly case-by-case 
-- to aid understanding; some of these could probably be
-- merged
unify' :: [Constraint] -> Either Error Sigma

unify' [] = return id         -- empty substitution

unify' tempC@((Constraint f1@(FunctionType s1 s2) f2@(FunctionType t1 t2)):constraints') = 
    unify' $ (Constraint s1 t1) : (Constraint s2 t2) : constraints'
    
unify' tempC@((Constraint t1@(TupleType ts1) t2@(TupleType ts2)):constraints') =
    if length ts1 == length ts2
    then unify' $ (zipWith Constraint ts1 ts2) ++ constraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two tuples of different sizes: " ++ show t1 ++ " and " ++ show t2
    
unify' tempC@((Constraint t1@(TypeConstructor n1 ts1) t2@(TypeConstructor n2 ts2)):constraints') =
    if n1 == n2
    then unify' $ (zipWith Constraint ts1 ts2) ++ constraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two type constructors of different names: " ++ show t1 ++ " and " ++ show t2
    
unify' tempC@((Constraint t1@(TypeConstant n1) t2@(TypeConstant n2)):constraints') =
    if n1 == n2
    then unify' constraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two different type constants: " ++ show t1 ++ " and " ++ show t2
    
unify' tempC@((Constraint t1@(TypeVariable n1) t2@(TypeVariable n2)):constraints') =
    if t1 == t2 
    then unify' constraints'
    else fmap (. substitution t1 t2) (unify' (substConstraints t1 t2 constraints'))
    
unify' tempC@((Constraint t1@(TypeVariable _) tc2@(TypeConstant _)):constraints') =
    fmap (. substitution t1 tc2) (unify' (substConstraints t1 tc2 constraints'))
    
unify' tempC@((Constraint s@(TypeVariable _) t):constraints') = 
    if not $ s `elem` (fv t)
    then fmap (. substitution s t) (unify' (substConstraints s t constraints'))
    else Left $ TypeInferenceFailure $ "Type inference failed due to recursive type constraint: " ++ show s ++ " = " ++ show t
    
-- Deal with the other way around by flipping
unify' tempC@((Constraint t1 t2@(TypeVariable _)):constraints') =
    unify' $ (Constraint t2 t1) : constraints'

    
    
-- And the catch-all; any other case should be a failure.
unify' tempC@((Constraint t1 t2) : constraints) = Left $ TypeInferenceFailure $ "Type inference failed on attempt to unify " ++ show t1 ++ " and " ++ show t2
    

    
{-
unify' tempC@((Constraint s t):constraints') = 
    trace ("Constraint contents " ++ show tempC) $
    if s == t
    then trace ("RULE 1") $ unify' constraints'
    else if not $ s `elem` (fv t)
    then trace ("RULE 2") $ fmap (. substitution s t) (unify' (substConstraints s t constraints'))
    else if not $ t `elem` (fv s)
    then trace ("RULE 3") $ fmap (. substitution t s) (unify' (substConstraints t s constraints'))
    else Left $ TypeInferenceFailure $ "Failure in unify on constraint " ++ show (Constraint s t)
-}





-- Could try and be efficient here - replace with a Set; no real need for now,
-- it's just looking for element presence either way
fv :: Type -> [Type]
fv (TypeConstant _) = []

fv tv@(TypeVariable _) = [tv]

fv (TypeConstructor _ ts) = foldr (++) [] (map fv ts)

fv (FunctionType f t) = fv f ++ fv t

fv (TupleType ts) = foldr (++) [] (map fv ts)



substConstraints :: Type -> Type -> [Constraint] -> [Constraint]
substConstraints toReplace replaceWith = map (substConstraint toReplace replaceWith)

substConstraint :: Type -> Type -> Constraint -> Constraint
substConstraint toReplace replaceWith (Constraint t1 t2) = 
    Constraint (substType toReplace replaceWith t1) (substType toReplace replaceWith t2)

substType :: Type -> Type -> Type -> Type
substType toReplace replaceWith t1 | toReplace == t1 = replaceWith

-- Don't need type constant or type variable; those are taken care of by
-- substType _ _ t1

substType toReplace replaceWith (TypeConstructor name ts) = 
    TypeConstructor name $ map (substType toReplace replaceWith) ts

substType toReplace replaceWith (FunctionType f t) = 
    FunctionType (substType toReplace replaceWith f) (substType toReplace replaceWith t)
    
substType toReplace replaceWith (TupleType ts) =
    TupleType $ map (substType toReplace replaceWith) ts
    
substType _ _ t1 = t1

    
    
    
-- A substitution will ONLY be applied to a type variable in position t1, since
-- that's how substitutions are created. So we have to recurse into t1', replacing
-- all occurrences within t1' of t1 with t2.
    
substitution :: Type -> Type -> Sigma
substitution t1 t2 tc@(TypeConstant _) = tc
    
substitution t1 t2 (TypeConstructor name ts) = TypeConstructor name $ map (substitution t1 t2) ts

substitution t1 t2 (FunctionType f t) = FunctionType (substitution t1 t2 f) (substitution t1 t2 t)

substitution t1 t2 (TupleType ts) = TupleType $ map (substitution t1 t2) ts

substitution t1 t2 t@(TypeVariable _) = if t1 == t 
                                        then t2
                                        else t

    

    
    
substitute :: Type -> Sigma -> Type
substitute tc@(TypeConstant _) _ = tc

substitute tv@(TypeVariable _) sigma = sigma tv

substitute (TypeConstructor name ts) sigma = TypeConstructor name $ map (flip substitute sigma) ts

substitute (FunctionType f t) sigma = FunctionType (substitute f sigma) (substitute t sigma)

substitute (TupleType ts) sigma = TupleType $ map (flip substitute sigma) ts



    
bindTypeVars :: Type -> TypeVarScope -> TypeVarScope
bindTypeVars t scope = 
    let free = fv t
    in foldr (\(TypeVariable tvName) newScope -> S.insert tvName newScope) scope free
    
    
    
lookupTV :: String -> State (Map String Type, FreshTV) (Maybe Type)
lookupTV name = do
    (typeVariables, _) <- get
    return $ typeVariables !? name
    
addTV :: String -> Type -> State (Map String Type, FreshTV) ()
addTV name t = modify' f
    where f (typeVariables, freshTV) = (insert name t typeVariables, freshTV)
    
    

incrementFresh :: Monad m => StateT (a, FreshTV) m ()
incrementFresh = modify' f
    where f (s1, freshTV) = (s1, getNextFreshTV freshTV)

   
nextFresh :: Monad m => StateT (a, FreshTV) m Type
nextFresh = do
    (_, freshTV) <- get
    let fr = getFresh freshTV
    incrementFresh
    return $ TypeVariable fr    

    
addConstraint :: Type -> Type -> StateError (Constraints, FreshTV) ()
addConstraint t1 t2 = modify' f
    where f (constraints, freshTV) = (S.insert (Constraint t1 t2) constraints, freshTV)

  
    
constrain :: Expr -> TypeEnv -> StateError (Constraints, FreshTV) Type

-- Variable rule 
constrain (Var (VarName name internalName)) gamma = 
    case (varTypes gamma) !? internalName of
        Just (GammaVarType t) -> return t
        Just (GammaTypeScheme boundTVs t) -> extractType (TypeScheme boundTVs t)
        Nothing -> lift $ Left $ TypeInferenceFailure $ "Type inference failed: could not find variable " ++ name

-- Literal rules
constrain (Lit NatLitZero) _ = return natType
constrain (Lit (BoolLit _)) _ = return boolType



-- Implicitly-typed abstraction rule
-- Assign a fresh type variable, check as typed
constrain (Lambda (LambdaVariable (TypedVar name UntypedVariable)) expr) gamma = do
    fr <- nextFresh
    constrain (Lambda (LambdaVariable (TypedVar name fr)) expr) gamma

constrain (Lambda (LambdaTuple (TypedTupleVar names UntypedVariable)) expr) gamma = do
    fr <- nextFresh
    constrain (Lambda (LambdaTuple (TypedTupleVar names fr)) expr) gamma
    
-- Abstraction rule
constrain l@(Lambda (LambdaVariable (TypedVar (VarName _ name) t1)) expr) gamma = do
    let varTypes' = insert name (GammaVarType t1) $ varTypes gamma
        boundTVs' = bindTypeVars t1 $ boundTVs gamma
    t2 <- constrain expr (TE varTypes' boundTVs')
    return $ FunctionType t1 t2
    

-- Abstraction rule for tuple variables
constrain (Lambda (LambdaTuple (TypedTupleVar names t1)) expr) gamma = do
    let tLen = length names
    ftvs <- replicateM tLen nextFresh
    let freshTuple = TupleType ftvs
    addConstraint t1 freshTuple
    -- need to bind both the type variables in t1 AND the fresh type variables generated
    -- i think
    let typePairs = zip (map internalName names) (map GammaVarType ftvs)
        varTypes' = foldr (uncurry insert) (varTypes gamma) typePairs
        boundTVs' = bindTypeVars freshTuple . bindTypeVars t1 $ boundTVs gamma
    t2 <- constrain expr (TE varTypes' boundTVs')
    return $ FunctionType t1 t2
    
    
-- If rule
-- Not sure about type equality on this, or how it's going to work in
-- unification - are we going to need to keep type variable scopes
-- around in one form or another?
constrain eif@(EIf ifExpr thenExpr elseExpr) gamma = do
    t1 <- constrain ifExpr gamma
    t2 <- constrain thenExpr gamma
    t3 <- constrain elseExpr gamma
    addConstraint t1 boolType
    addConstraint t2 t3
    return $ t2

    
-- Application rule
constrain app@(Application e1 e2) gamma = do
    t1 <- constrain e1 gamma
    t2 <- constrain e2 gamma
    x <- nextFresh
    addConstraint t1 (FunctionType t2 x)
    return x


-- Tuple rule
constrain (ETup ts) gamma = do
    ts' <- mapM (flip constrain gamma) ts
    return $ TupleType ts'


-- Tuple accessor rule, taken straight from the old and adjusted for StateT
-- We've had to alter tuples a bit to take into account that the .i method
-- doesn't play well with inference.
{-
check (ETupAcc expr i tupleSize) gamma = do
    t1 <- check expr gamma
    case t1 of
        TupleType lT -> let i' = i - 1         -- tuples are 1-indexed, unlike lists
                        in if i' >= length lT  -- don't need to check -ve, since '-' not accepted by parser; also, 0 on its own is the only Nat literal so won't be parsed as a tuple accessor.
                           then lift $ Left $ TypeInferenceFailure $ "Type-check failed: tuple index of " ++ show i ++ " too large for tuple of length " ++ show (length lT) ++ " (type " ++ show (TupleType lT) ++ ")"
                           else return $ lT !! i'
        _ -> lift $ Left $ TypeInferenceFailure $ "Type-check failed: tuple accessor ." ++ show i ++ " given " ++ show t1
-}
constrain (ETupAcc expr i tupleSize) gamma = do
    t1 <- constrain expr gamma
    (tupleType, typeAtIndex) <- emptyTupleAndIndex tupleSize i
    addConstraint t1 tupleType
    return typeAtIndex


-- Let rule
-- we use Ch. 22 p.331-336 on this. Simple version.
-- G |- [x|->t1]t2 : T2    G |- t1 : T1
-- ------------------------------------
-- G |- let x=t1 in t2 : T2

-- Changed to 'complicated' version - we check the type of bindingExpr, and store in gamma as a
-- type context set to the current TV scope. pp.333-334 of Pierce (2002)
constrain (ELet (LetVar var) bindingExpr inExpr) gamma = do
    t1 <- constrain bindingExpr gamma
    (constraints, _) <- get
    sigma <- lift $ unify constraints -- don't forget to unify and calculate a principal type!
    let boundTVs' = updateBoundTVsFromSubstitution (boundTVs gamma) sigma
        t1' = substitute t1 sigma
        varTypes' = insert (internalName var) (GammaTypeScheme boundTVs' t1') $ varTypes gamma
    t2 <- constrain inExpr (TE varTypes' boundTVs')
    return t2       -- being explicit here

    
    
    
-- Let rule for tuple deconstructor
constrain (ELet (LetTuple vars) bindingExpr inExpr) gamma = do
    t1 <- constrain bindingExpr gamma
    emptyTup <- (emptyTuple $ length vars)
    addConstraint t1 emptyTup
    (constraints, _) <- get
    sigma <- lift $ unify constraints
    let t1' = substitute t1 sigma
        boundTVs' = updateBoundTVsFromSubstitution (boundTVs gamma) sigma
    case t1' of
        TupleType ts' -> 
            let f :: (VarName, Type) -> Map String GammaElem -> Map String GammaElem
                --f (vName, t) varTypes = insert (internalName vName) (GammaTypeScheme (boundTVs gamma) t) varTypes
                f (vName, t) varTypes = insert (internalName vName) (GammaTypeScheme boundTVs' t) varTypes
                varTypes' = foldr f (varTypes gamma) (zip vars ts')
            in do
              t2 <- constrain inExpr (TE varTypes' boundTVs')
              return $ t2       -- again, make it explicit
        -- We should never get this
        _ -> lift $ Left $ TypeInferenceFailure $ "Type inference failed: let expression expected tuple type in " ++ show bindingExpr
    

-- looks at a type variable scope and a substitution
-- for every bound type variable T, where the substitute T=X exists,
-- add fv(X) to the bound type variables
updateBoundTVsFromSubstitution :: TypeVarScope -> Sigma -> TypeVarScope
updateBoundTVsFromSubstitution boundTV sigma = 
    let listNewBound = S.toList boundTV >>= (\tName -> fv . sigma $ TypeVariable tName) 
        newBoundNames = map (\(TypeVariable name) -> name) listNewBound
    in boundTV `S.union` S.fromList newBoundNames

    
-- returns (tupleType, typeAtIndex)
emptyTupleAndIndex :: Int -> Int -> StateError (Constraints, FreshTV) (Type, Type)
emptyTupleAndIndex size index = do
    ts <- replicateM size nextFresh
    return (TupleType ts, ts !! (index - 1))


emptyTuple size = do
    ts <- replicateM size nextFresh
    return $ TupleType ts
    

-- FROM PREVIOUS


-- We consider two types 'equal' if their structures match recursively,
-- and their type variables are identical up to renaming, ignoring bound/free
-- variable state (think this is ok here).
typeEqualsRename :: TypeVarScope -> Type -> Type -> Bool
typeEqualsRename boundTVs t1 t2 = evalState (typeEqualsRename' boundTVs t1 t2) (empty, empty)

-- We need an equivalence of types, not an implication -
-- T1 <==> T2
-- which means a dual map

typeEqualsRename' :: TypeVarScope -> Type -> Type -> State (Map String Type, Map String Type) Bool

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
                                    
                                    
                                    
writeTypeConstraintsToFile2 :: String -> Constraints -> IO ()
writeTypeConstraintsToFile2 filePath constraints = do
    let constraintStr = S.map show constraints
        fullFile = intercalate "\n" $ S.toList constraintStr
    writeFile filePath fullFile
                                    
                                    
                                    
                                    
                                    
-- For curiosity's sake : Algorithm W from Damas and Milner (1982) Principal type-schemes for functional programs
-- We kind of have to 'invert' what's going on with "bound" type variables - in the original, 
-- all type variables appearing in Gamma (or A) are considered free, and if they're free in A, then
-- they're not free on the RHS of |-. But a type scheme contains \/a1,a2,...,an.t where a1...an are
-- free variables of t. And these are computed from those that are NOT free in A at time of definition.
-- So we're effectively listing those type variables that are 'free', not those that are 'bound' (by my
-- old definitions of those words).
{-
data WTypeScheme = WTypeScheme [String] Type

type WTypeEnv = Map String WTypeScheme


wRename :: WTypeScheme -> StateError ((), FreshTV) (Sigma, Type)
wRename = undefined

wSubstitute :: WTypeScheme -> Sigma -> WTypeScheme
wSubstitute = undefined

w :: Expr -> WTypeEnv -> StateError ((), FreshTV) (Sigma, Type)

w (Var vName) a = 
    case a !? (internalName vName) of
        Just wts -> wRename wts
        Nothing -> lift $ Left $ TypeInferenceError $ "In W: could not find variable " ++ codeName vName


w (Application e1 e2) a = do
    (s1, t1) <- w e1 a
    (s2, t2) <- w e2 (wSubstitute t1 s1)
    beta <- nextFresh
    v <- lift $ unify $ S.fromList [Constraint (wSubstitute t1 s2) (FunctionType t2 beta)]
    return (v . s2 . s1, wSubstitute beta v)
-}
