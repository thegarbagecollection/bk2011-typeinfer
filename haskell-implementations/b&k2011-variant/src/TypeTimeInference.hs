module TypeTimeInference where


import Tokens
import Renaming
import Errors
import Data.Either
import Data.Map.Strict (empty, insert, (!?), Map, fromList, (!), fromSet)
import qualified Data.Set as S
import qualified ConstraintSolve as CS
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
data TypeEnv = TypeEnv { varTypes :: Map String GammaElem, getBoundTypeVars :: TypeVarScope }

runInference :: Either Error Expr -> Either Error (Type, TimeStep)
runInference e = e >>= inferType

-- A constraint (T1, T2) can be interpreted as T1=T2
type TypeConstraints = S.Set TypeConstraint

data TypeConstraint = TypeConstraint Type Type deriving (Eq, Ord)



instance Show TypeConstraint where
    show (TypeConstraint t1 t2) = show t1 ++ " = " ++ show t2

writeTypeConstraintsToFile :: String -> TypeConstraints -> IO ()
writeTypeConstraintsToFile fileName constraints = do
    let constraintStr = S.map show constraints
        fullFile = intercalate "\n" $ S.toList constraintStr
    writeFile ("constraints/" ++ fileName ++ ".type") fullFile
    
writeTypeConstraintsToFile2 :: String -> TypeConstraints -> IO ()
writeTypeConstraintsToFile2 filePath constraints = do
    let constraintStr = S.map show constraints
        fullFile = intercalate "\n" $ S.toList constraintStr
    writeFile filePath fullFile

    
type TimeConstraints = S.Set TimeConstraint
type TimeConstraint = CS.InputConstraint
    
    
-- We use the same fresh type variable generator as in the previous version - just a stream
-- of ?T1,...,?Tk,... where  1.. is the infinite stream of integers
data FreshTypeVarGen = FreshTypeVarGen { getFreshTypeVar :: String, getNextFreshTypeVarGen :: FreshTypeVarGen }

freshTypeVar :: Integer -> FreshTypeVarGen
freshTypeVar n = FreshTypeVarGen ("?T" ++ show n) (freshTypeVar $ n + 1)


data FreshTimeVarGen = FreshTimeVarGen { getFreshTimeVar :: String, getNextFreshTimeVarGen :: FreshTimeVarGen }
freshTimeVar :: Integer -> FreshTimeVarGen
freshTimeVar n = FreshTimeVarGen ("?X" ++ show n) (freshTimeVar $ n + 1)



data TypeScheme = TypeScheme { boundInTypeScheme :: TypeVarScope, typeInTypeScheme :: Type }



-- TODO: this is basically the same as rename; we can redefine rename
-- as this, but ensuring that the type scheme contains no bound TVs
-- Given a type scheme, returns a type where all non-bound type variables are fresh
extractType :: TypeScheme -> StateError ConstraintState Type 

extractType (TypeScheme boundTypeVars typeToRename) = do
    freshTypeVarGen <- gets getFreshTypeVarGen
    let startingBindings = fromSet (\name -> TypeVariable name) boundTypeVars
        (renamed, (_, freshTypeVarGen')) = runState (extractType' typeToRename) (startingBindings, freshTypeVarGen)
    modify' (\cs -> cs { getFreshTypeVarGen = freshTypeVarGen' })
    return renamed


extractType' :: Type -> State (Map String Type, FreshTypeVarGen) Type
    
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
            (_, ftvg) <- get
            let tv' = TypeVariable . getFreshTypeVar $ ftvg
                ftvg' = getNextFreshTypeVarGen ftvg
            modify' (\(m, _) -> (m, ftvg'))
            updateBindings n tv'
            return tv'
            
extractType' (BulletType t) = do
    t' <- extractType' t
    return $ BulletType t'
            
            
updateBindings :: String -> Type -> State (Map String Type, FreshTypeVarGen) ()
updateBindings name t = modify' f
    where f (bindings, freshTypeVarGen) = (insert name t bindings, freshTypeVarGen)
    
emptyConstraintState = ConstraintState S.empty (freshTypeVar 1) S.empty (freshTimeVar 1)

constraintStateFromTimes times = ConstraintState S.empty (freshTypeVar 1) times (freshTimeVar 1)

constraintsTemp :: Either Error Expr -> Either Error (TimedType, TypeConstraints, TimeConstraints)
constraintsTemp e = do
    expr <- e
    (timedType, cs) <- runStateT (constrain expr (TypeEnv builtInAbsType S.empty)) emptyConstraintState
    return $ (timedType, getTypeConstraints cs, getTimeConstraints cs)

    
-- returns pre-unification type, type constraints, time constraints
constraintsTempFromGammaAndTimeConstraints :: Map String GammaElem -> TimeConstraints -> Either Error Expr -> Either Error (TimedType, TypeConstraints, TimeConstraints)
constraintsTempFromGammaAndTimeConstraints gammaInit timeConstraintsInit e = do
    expr <- e
    (timedType, cs) <- runStateT (constrain expr (TypeEnv gammaInit S.empty)) (constraintStateFromTimes timeConstraintsInit)
    return $ (timedType, getTypeConstraints cs, getTimeConstraints cs)
    
    
type TimeStep = Integer    

-- returns the type and the timestep of the root expression
inferType :: Expr -> Either Error (Type, TimeStep)
inferType expr = do
        -- (resultType, (typeConstraints, _)) <- runStateT (constrain expr (TypeEnv builtInAbsType S.empty))  emptyConstraintState
        ((t, timeVar), cs) <- runStateT (constrain expr (TypeEnv builtInAbsType S.empty))  emptyConstraintState
        sigma <- unify . getTypeConstraints $ cs
        timeVal <- solveTime timeVar $ getTimeConstraints cs
        return $ (substitute t sigma, timeVal)
                      

                      
-- REPL thing!!                    ************************************************************************************************************************************************************************************************************************************************************************
inferTypeFromGammaAndTimeConstraints :: Expr -> Map String GammaElem -> TimeConstraints -> Either Error (Type, TimeStep)
inferTypeFromGammaAndTimeConstraints expr gammaInit timeConstraintsInit = do
        ((t, timeVar), cs) <- runStateT (constrain expr (TypeEnv gammaInit S.empty))  (constraintStateFromTimes timeConstraintsInit)
        sigma <- unify . getTypeConstraints $ cs
        timeVal <- solveTime timeVar $ getTimeConstraints cs
        return $ (substitute t sigma, timeVal)
                      
-- Solve time constraints to get the time for a given time variable
solveTime :: TimeVar -> TimeConstraints -> Either Error TimeStep
solveTime timeVar timeConstraints = do
    let tcL = S.toList timeConstraints
    case CS.solveInputConstraints tcL of
        Right mapVarInt -> case mapVarInt !? timeVar of
                                    Just time -> Right time
                                    Nothing -> Left . TimeConstraintFailure $ "Time constraint failed: variable " ++ timeVar ++ " not found in returned variable map."
        Left solverErr -> Left . TimeConstraintFailure $ CS.solverFailureToString solverErr
    

                   
unify :: TypeConstraints -> Either Error Sigma
unify typeConstraints = 
    let typeConstraintList = S.toList typeConstraints
    in unify' typeConstraintList

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
unify' :: [TypeConstraint] -> Either Error Sigma

unify' [] = return id         -- empty substitution

unify' ((TypeConstraint (FunctionType s1 s2) (FunctionType t1 t2)):typeConstraints') = 
    unify' $ (TypeConstraint s1 t1) : (TypeConstraint s2 t2) : typeConstraints'
    
unify' ((TypeConstraint t1@(TupleType ts1) t2@(TupleType ts2)):typeConstraints') =
    if length ts1 == length ts2
    then unify' $ (zipWith TypeConstraint ts1 ts2) ++ typeConstraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two tuples of different sizes: " ++ show t1 ++ " and " ++ show t2
    
unify' ((TypeConstraint t1@(TypeConstructor n1 ts1) t2@(TypeConstructor n2 ts2)):typeConstraints') =
    if n1 == n2
    then unify' $ (zipWith TypeConstraint ts1 ts2) ++ typeConstraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two type constructors of different names: " ++ show t1 ++ " and " ++ show t2
    
unify' ((TypeConstraint t1@(TypeConstant n1) t2@(TypeConstant n2)):typeConstraints') =
    if n1 == n2
    then unify' typeConstraints'
    else Left $ TypeInferenceFailure $ "Type inference failed: tried to unify two different type constants: " ++ show t1 ++ " and " ++ show t2
    
unify' ((TypeConstraint t1@(TypeVariable n1) t2@(TypeVariable n2)):typeConstraints') =
    if t1 == t2 
    then unify' typeConstraints'
    else fmap (. substitution t1 t2) (unify' (substTypeConstraints t1 t2 typeConstraints'))
    
unify' ((TypeConstraint t1@(TypeVariable _) tc2@(TypeConstant _)):typeConstraints') =
    fmap (. substitution t1 tc2) (unify' (substTypeConstraints t1 tc2 typeConstraints'))
    
unify' ((TypeConstraint s@(TypeVariable _) t):typeConstraints') = 
    if not $ s `elem` (fv t)
    then fmap (. substitution s t) (unify' (substTypeConstraints s t typeConstraints'))
    else Left $ TypeInferenceFailure $ "Type inference failed due to recursive type constraint: " ++ show s ++ " = " ++ show t
    
-- Deal with the other way around by flipping
unify' ((TypeConstraint t1 t2@(TypeVariable _)):typeConstraints') =
    unify' $ (TypeConstraint t2 t1) : typeConstraints'

-- Bullet types?
-- obviously *T = *U will match iff T = U, so it's a wrapper-stripper like TypeConstructor
-- any other case will fail except T = *U or *U = T, but both of those are dealt with already by TypeVariable
unify' ((TypeConstraint (BulletType t1) (BulletType t2)):typeConstraints') = unify' $ (TypeConstraint t1 t2):typeConstraints'
    

-- And the catch-all; any other case should be a failure.
unify' ((TypeConstraint t1 t2) : typeConstraints) = Left $ TypeInferenceFailure $ "Type inference failed on attempt to unify " ++ show t1 ++ " and " ++ show t2
    

    
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
-- `Free variables', or, all type variables in a type
fv :: Type -> [Type]
fv (TypeConstant _) = []

fv tv@(TypeVariable _) = [tv]

fv (TypeConstructor _ ts) = foldr (++) [] (map fv ts)

fv (FunctionType f t) = fv f ++ fv t

fv (TupleType ts) = foldr (++) [] (map fv ts)

fv (BulletType t) = fv t

-- an immediate substitution of one type for another within a list of type constraints. 
-- Used by unify as an `immediate sigma' on the remainder to be recursed on
substTypeConstraints :: Type -> Type -> [TypeConstraint] -> [TypeConstraint]
substTypeConstraints toReplace replaceWith = map (substTypeConstraint toReplace replaceWith)

substTypeConstraint :: Type -> Type -> TypeConstraint -> TypeConstraint
substTypeConstraint toReplace replaceWith (TypeConstraint t1 t2) = 
    TypeConstraint (substType toReplace replaceWith t1) (substType toReplace replaceWith t2)

substType :: Type -> Type -> Type -> Type                           -- only used in substTypeConstraints! 
substType toReplace replaceWith t1 | toReplace == t1 = replaceWith

-- Don't need type constant or type variable; those are taken care of by
-- substType _ _ t1

substType toReplace replaceWith (TypeConstructor name ts) = 
    TypeConstructor name $ map (substType toReplace replaceWith) ts

substType toReplace replaceWith (FunctionType f t) = 
    FunctionType (substType toReplace replaceWith f) (substType toReplace replaceWith t)
    
substType toReplace replaceWith (TupleType ts) =
    TupleType $ map (substType toReplace replaceWith) ts
    
substType toReplace replaceWith (BulletType t) =
    BulletType $ substType toReplace replaceWith t
    
substType _ _ t1 = t1

    
    
    
-- A substitution will ONLY be applied to a type variable in position t1, since
-- that's how substitutions are created. So we have to recurse into t1', replacing
-- all occurrences within t1' of t1 with t2.
    
substitution :: Type -> Type -> Sigma               -- creates a substitution function sigma (\T -> [X|->U]T)
substitution t1 t2 tc@(TypeConstant _) = tc
    
substitution t1 t2 (TypeConstructor name ts) = TypeConstructor name $ map (substitution t1 t2) ts

substitution t1 t2 (FunctionType f t) = FunctionType (substitution t1 t2 f) (substitution t1 t2 t)

substitution t1 t2 (TupleType ts) = TupleType $ map (substitution t1 t2) ts

substitution t1 t2 t@(TypeVariable _) = if t1 == t 
                                        then t2
                                        else t

    
substitution t1 t2 (BulletType t) = BulletType $ substitution t1 t2 t
    
    
substitute :: Type -> Sigma -> Type                 -- applies a substitution function sigma  (\T -> [X|->U]T) T2
substitute tc@(TypeConstant _) _ = tc

substitute tv@(TypeVariable _) sigma = sigma tv

substitute (TypeConstructor name ts) sigma = TypeConstructor name $ map (flip substitute sigma) ts

substitute (FunctionType f t) sigma = FunctionType (substitute f sigma) (substitute t sigma)

substitute (TupleType ts) sigma = TupleType $ map (flip substitute sigma) ts

substitute (BulletType t) sigma = BulletType $ substitute t sigma


    
bindTypeVars :: Type -> TypeVarScope -> TypeVarScope        -- adds all type variables in t to the type variable scope, binding them
bindTypeVars t scope = 
    let free = fv t
    in foldr (\(TypeVariable tvName) newScope -> S.insert tvName newScope) scope free
    
    
{- NOT USED 
lookupTypeVar :: String -> State (Map String Type, FreshTypeVarGen) (Maybe Type)
lookupTypeVar name = do
    (typeVariables, _) <- get
    return $ typeVariables !? name
    
addTypeVar :: String -> Type -> State (Map String Type, FreshTypeVarGen) ()
addTypeVar name t = modify' f
    where f (typeVariables, freshTypeVarGen) = (insert name t typeVariables, freshTypeVarGen)
-}
    

incrementFreshTypeVar :: StateError ConstraintState ()
incrementFreshTypeVar = modify' f
    where f cs = let ftvg' = getNextFreshTypeVarGen . getFreshTypeVarGen $ cs
                 in cs { getFreshTypeVarGen = ftvg' }

   
nextFreshTypeVar :: StateError ConstraintState Type
nextFreshTypeVar = do
    freshTypeVarGen <- gets getFreshTypeVarGen
    let fr = getFreshTypeVar freshTypeVarGen
    incrementFreshTypeVar
    return $ TypeVariable fr    

    
    

    
    
data ConstraintState = ConstraintState { getTypeConstraints::TypeConstraints,
                                         getFreshTypeVarGen::FreshTypeVarGen, 
                                         getTimeConstraints::TimeConstraints, 
                                         getFreshTimeVarGen::FreshTimeVarGen }
    
addTypeConstraint :: Type -> Type -> StateError ConstraintState ()
addTypeConstraint t1 t2 = modify' f
    where f cs = let oldTypeC = getTypeConstraints cs 
                     newTypeC = S.insert (TypeConstraint t1 t2) oldTypeC
                 in cs { getTypeConstraints = newTypeC }



data ConstraintType = CEQ | CLEQ | CGEQ
    
    
addTimeConstraint :: (TimeVar, Integer) -> ConstraintType -> (TimeVar, Integer) -> StateError ConstraintState ()
addTimeConstraint (var1, summand1) constraintType (var2, summand2) = modify' f
    where f cs = let oldTimeC = getTimeConstraints cs
                     constraintConstructor = case constraintType of
                                                CEQ -> CS.EQConstraintI
                                                CLEQ -> CS.LEQConstraintI
                                                CGEQ -> CS.GEQConstraintI
                     newTimeC = S.insert (constraintConstructor (CS.Expr var1 summand1) (CS.Expr var2 summand2)) oldTimeC
                 in cs { getTimeConstraints = newTimeC }

    
nextFreshTimeVar :: StateError ConstraintState TimeVar
nextFreshTimeVar = do
    constraintState <- get
    let ftvg = getFreshTimeVarGen constraintState
        tv = getFreshTimeVar ftvg
        ftvg' = getNextFreshTimeVarGen ftvg
    put $ constraintState { getFreshTimeVarGen = ftvg' }
    return tv

    

    
    
constrain :: Expr -> TypeEnv -> StateError ConstraintState TimedType
-- Variable rule 
constrain (Var (VarName name internalName)) gamma = do
    newTimeVar <- nextFreshTimeVar
    case (varTypes gamma) !? internalName of
        Just (GammaVarType (typ, timeVar)) -> do
            addTimeConstraint (newTimeVar, 0) CGEQ (timeVar, 0)
            return (typ, newTimeVar)
            
        Just (GammaTypeScheme boundTypeVars (typ, timeVar)) -> do
            addTimeConstraint (newTimeVar, 0) CGEQ (timeVar, 0)
            newTyp <- extractType (TypeScheme boundTypeVars typ)
            return (newTyp, newTimeVar)
            
        Nothing -> lift $ Left $ TypeInferenceFailure $ "Type inference failed: could not find variable " ++ name

-- Literal rules
constrain (Lit NatLitZero) _ = do
    newTimeVar <- nextFreshTimeVar
    addTimeConstraint (newTimeVar, 1) CGEQ (newTimeVar, 0) -- kludge it - got to have a constraint! or it won't put the variable in the graph
    return (natType, newTimeVar)
    
constrain (Lit (BoolLit _)) _ = do
    newTimeVar <- nextFreshTimeVar
    addTimeConstraint (newTimeVar, 1) CGEQ (newTimeVar, 0) -- kludge it - got to have a constraint! or it won't put the variable in the graph
    return (boolType, newTimeVar)

    
-- Bullet / await rules
constrain (EBullet expr) gamma = do
    newTimeVar <- nextFreshTimeVar
    (typ, timeVar) <- constrain expr gamma
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar, (-1))
    return (BulletType typ, newTimeVar)
    

constrain (EAwait expr) gamma = do
    newTimeVar <- nextFreshTimeVar
    newTypeVar <- nextFreshTypeVar
    (typ, timeVar) <- constrain expr gamma
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar, 1)
    addTypeConstraint typ (BulletType newTypeVar)
    return (newTypeVar, newTimeVar)

-- Implicitly-typed abstraction rule
-- Assign a fresh type variable, check as typed
constrain (Lambda (LambdaVariable (TypedVar name UntypedVariable)) expr) gamma = do
    fr <- nextFreshTypeVar
    constrain (Lambda (LambdaVariable (TypedVar name fr)) expr) gamma

constrain (Lambda (LambdaTuple (TypedTupleVar names UntypedVariable)) expr) gamma = do
    fr <- nextFreshTypeVar
    constrain (Lambda (LambdaTuple (TypedTupleVar names fr)) expr) gamma
    
-- Abstraction rule
constrain l@(Lambda (LambdaVariable (TypedVar (VarName _ name) t1)) expr) gamma = do
    newTimeVar <- nextFreshTimeVar
    let varTypes' = insert name (GammaVarType (t1, newTimeVar)) $ varTypes gamma
        boundTypeVars' = bindTypeVars t1 $ getBoundTypeVars gamma
    (t2, timeVar) <- constrain expr (TypeEnv varTypes' boundTypeVars')
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar, 0)
    return $ (FunctionType t1 t2, newTimeVar)
    

-- Abstraction rule for tuple variables
-- how do we do tuple abstractions here?
-- i think for a tuple of type { T1, ..., Tn } we need
{-

G, x1:K T1,..., xn:K Tn |- e :i T2'
K fresh time variable
T1, Tn fresh type vars
where T1' = {T1,...,Tn}
--------------------------------------------------
G |- \{x1,...,xn}.e :K T1'->T2' | (C, M u {K = i })              might be K >= i? probably not though
-}
constrain (Lambda (LambdaTuple (TypedTupleVar names t1)) expr) gamma = do
    let tLen = length names
    ftvs <- replicateM tLen nextFreshTypeVar
    newTimeVar <- nextFreshTimeVar
    let freshTuple = TupleType ftvs
    addTypeConstraint t1 freshTuple
    -- need to bind both the type variables in t1 AND the fresh type variables generated
    -- i think
    let typePairs = zip (map internalName names) (map GammaVarType $ zip ftvs $ repeat newTimeVar)
        varTypes' = foldr (uncurry insert) (varTypes gamma) typePairs
        boundTypeVars' = bindTypeVars freshTuple . bindTypeVars t1 $ getBoundTypeVars gamma
    (t2, timeVar) <- constrain expr (TypeEnv varTypes' boundTypeVars')
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar, 0)
    return $ (FunctionType t1 t2, newTimeVar)
    
    
    
    
 
-- Application rule
constrain app@(Application e1 e2) gamma = do
    (t1, timeVar1) <- constrain e1 gamma
    (t2, timeVar2) <- constrain e2 gamma
    x <- nextFreshTypeVar
    addTypeConstraint t1 (FunctionType t2 x)
    addTimeConstraint (timeVar1, 0) CEQ (timeVar2, 0)
    return (x, timeVar1)


-- Tuple rule
constrain (ETup exs) gamma = do
    (types', times') <- unzip <$> mapM (flip constrain gamma) exs
    newTimeVar <- nextFreshTimeVar
    mapM_ (addTimeConstraint (newTimeVar, 0) CEQ) $ zip times' $ repeat 0
    return $ (TupleType types', newTimeVar)


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
    (t1, timeVar) <- constrain expr gamma
    (tupleType, typeAtIndex) <- emptyTupleAndIndex tupleSize i
    addTypeConstraint t1 tupleType
    return (typeAtIndex, timeVar)


    
-- If rule
-- not sure about the time constraints here. either everything needs to be the same,
-- or the results can be delayed so it's the maximum of all of the expressions
-- Going to run with equality for now, see if it allows parsing of map etc in B&K
-- what we might be able to say is that we need K = i2 = i3, but i1 <= i2, i1 <= i3
-- but for now...
{-
G |- e1 :i1 T1      G |- e2 :i2 T2      G |- e3 :i3 T3
fresh time var K
M' = M u { K = i1, K = i2, K = i3 }
C' = C u { T1 = Bool, T2 = T3 }
-------------------------------------------------------
G |- if e1 then e2 else e3 :K T2 | (C', M')
-}
constrain eif@(EIf ifExpr thenExpr elseExpr) gamma = do
    (t1, timeVar1) <- constrain ifExpr gamma
    (t2, timeVar2) <- constrain thenExpr gamma
    (t3, timeVar3) <- constrain elseExpr gamma
    newTimeVar <- nextFreshTimeVar
    addTypeConstraint t1 boolType
    addTypeConstraint t2 t3
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar1, 0)
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar2, 0)
    addTimeConstraint (newTimeVar, 0) CEQ (timeVar3, 0)
    return $ (t2, newTimeVar)
    
-- OLD Let rule
-- we use Ch. 22 p.331-336 on this. Simple version.
-- G |- [x|->t1]t2 : T2    G |- t1 : T1
-- ------------------------------------
-- G |- let x=t1 in t2 : T2

-- Changed to 'complicated' version - we check the type of bindingExpr, and store in gamma as a
-- type context set to the current TV scope. pp.333-334 of Pierce (2002)

{-
Now we need to update this for time variables. Not easy. Also can't remember the notation.
I'm going to guess that the body has to be of time >= the bound expression, also that the time
of the body is the time of the full expression. do we also want the variable to be available at 
whatever time?

G |- [x:K2 T1|->e1]e2 :i T2    G |- t1 :j T1

K1 is fresh time variable (for return)
K2 is fresh time variable (for binding variable in let body)


K1 = i, i >= j, i >= K2, K2 >= j             
maybe???? so we're saying that 
- the variable is available any time after e1's time, 
- the let body is available any time after the variable's time
- the result is available at the time of the body
although variable accesses are able to be delayed by default, so we can just set var's time to j, and the rest takes care of itself
K1 = i, i >= j, K2 = j

M' = M u { K1 = i, i >= j, K2 = j }
also the return type is just the type of e2
we could just return type and time, but eh....

ok so that doesn't work. instead we're going to do
M' = M u { K1 = i, K2 = j }
and leave out the constraint that the let-bound must be <= let-in
which sort of makes sense - if you've got a thing that arrives at i+1, you can produce a thing that arrives at i as long as
the i+1 is respected.
------------------------------------
G |- let x=e1 in e2 :K1 T2 
-}
constrain (ELet (LetVar var) bindingExpr inExpr) gamma = do
    --timeResult <- nextFreshTimeVar
    --timeVarBound <- nextFreshTimeVar
    (t1, timeBound) <- constrain bindingExpr gamma
    typeConstraints <- gets getTypeConstraints
    sigma <- lift $ unify typeConstraints -- don't forget to unify and calculate a principal type!
    let boundTypeVars' = updateBoundTypeVarsFromSubstitution (getBoundTypeVars gamma) sigma
        t1' = substitute t1 sigma
        varTypes' = insert (internalName var) (GammaTypeScheme boundTypeVars' (t1', timeBound)) $ varTypes gamma
    (t2, timeIn) <- constrain inExpr (TypeEnv varTypes' boundTypeVars')
    return (t2, timeIn)       

    
    
    
-- Let rule for tuple deconstructor
-- using the same rules as above for time constraints, pretty much
-- M' = M u { K1 = i, i >= j, K2 = j }
constrain (ELet (LetTuple vars) bindingExpr inExpr) gamma = do
    --timeResult <- nextFreshTimeVar
    --timeVarBound <- nextFreshTimeVar
    (t1, timeBound) <- constrain bindingExpr gamma
    emptyTup <- (emptyTuple $ length vars)
    addTypeConstraint t1 emptyTup
    typeConstraints <- gets getTypeConstraints
    sigma <- lift $ unify typeConstraints
    let t1' = substitute t1 sigma
        boundTypeVars' = updateBoundTypeVarsFromSubstitution (getBoundTypeVars gamma) sigma
    case t1' of
        TupleType ts' -> 
            let f :: (VarName, Type) -> Map String GammaElem -> Map String GammaElem
                f (vName, t) varTypes = insert (internalName vName) (GammaTypeScheme boundTypeVars' (t, timeBound)) varTypes
                varTypes' = foldr f (varTypes gamma) (zip vars ts')
            in do
              (t2, timeIn) <- constrain inExpr (TypeEnv varTypes' boundTypeVars')
              --addTimeConstraint (timeResult, 0) CEQ (timeIn, 0)
              --addTimeConstraint (timeBound, 0) CEQ (timeVarBound, 0)
              return $ (t2, timeIn)       -- again, make it explicit
        -- We should never get this
        _ -> lift $ Left $ TypeInferenceFailure $ "Type inference failed: let expression expected tuple type in " ++ show bindingExpr
    

-- looks at a type variable scope and a substitution
-- for every bound type variable T, where the substitute T=X exists,
-- add fv(X) to the bound type variables
updateBoundTypeVarsFromSubstitution :: TypeVarScope -> Sigma -> TypeVarScope
updateBoundTypeVarsFromSubstitution boundTV sigma = 
    let listNewBound = S.toList boundTV >>= (\tName -> fv . sigma $ TypeVariable tName) 
        newBoundNames = map (\(TypeVariable name) -> name) listNewBound
    in boundTV `S.union` S.fromList newBoundNames

    
-- returns (tupleType, typeAtIndex)
emptyTupleAndIndex :: Int -> Int -> StateError ConstraintState (Type, Type)
emptyTupleAndIndex size index = do
    ts <- replicateM size nextFreshTypeVar
    return (TupleType ts, ts !! (index - 1))


emptyTuple size = do
    ts <- replicateM size nextFreshTypeVar
    return $ TupleType ts
    

    
-- FROM PREVIOUS


-- We consider two types 'equal' if their structures match recursively,
-- and their type variables are identical up to renaming, ignoring bound/free
-- variable state (think this is ok here).
typeEqualsRename :: TypeVarScope -> Type -> Type -> Bool
typeEqualsRename boundTypeVars t1 t2 = evalState (typeEqualsRename' boundTypeVars t1 t2) (empty, empty)

-- We need an equivalence of types, not an implication -
-- T1 <==> T2
-- which means a dual map

typeEqualsRename' :: TypeVarScope -> Type -> Type -> State (Map String Type, Map String Type) Bool

typeEqualsRename' boundTypeVars (TypeConstant c1) (TypeConstant c2) = return $ c1 == c2

typeEqualsRename' boundTypeVars (FunctionType f1 t1) (FunctionType f2 t2) = do
    fEq <- typeEqualsRename' boundTypeVars f1 f2
    tEq <- typeEqualsRename' boundTypeVars t1 t2
    return $ fEq && tEq
    
typeEqualsRename' boundTypeVars (TupleType ts1) (TupleType ts2) = do
    eqs <- mapM (uncurry $ typeEqualsRename' boundTypeVars) (zip ts1 ts2)
    return $ length ts1 == length ts2 && and eqs

typeEqualsRename' boundTypeVars (TypeConstructor n1 ts1) (TypeConstructor n2 ts2) = do
    eqs <- mapM (uncurry $ typeEqualsRename' boundTypeVars) (zip ts1 ts2)
    return $ n1 == n2 && and eqs


typeEqualsRename' boundTypeVars tv1@(TypeVariable name1) tv2@(TypeVariable name2) = do
    -- lookupTable1 maps the name of type variable 1 to a type variable 2 to which it's bound
    -- vice versa for lookupTable2
    case (isTypeVariableBound name1 boundTypeVars, isTypeVariableBound name2 boundTypeVars) of
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
