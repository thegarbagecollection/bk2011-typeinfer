{-# LANGUAGE FlexibleContexts #-}

module ConstraintSolve where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Data.List
-- import Control.Monad.State.Strict
import Control.Monad
import Data.Foldable
import Data.Traversable
import Data.Either
import Data.Maybe

import Control.Monad.Trans
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Maybe



data Expr = Expr Variable Integer deriving (Eq, Ord, Show)
type Variable = String
data InputConstraint = LEQConstraintI Expr Expr
                     | EQConstraintI Expr Expr 
                     | GEQConstraintI Expr Expr deriving (Eq, Ord, Show)

data Constraint = LEQConstraint Expr Expr deriving (Eq, Ord, Show)

                
newtype Vertex a = Vertex a deriving (Eq, Ord, Show)

data AdjList a = AdjList { incoming::M.Map (Vertex a) [Edge a], outgoing::M.Map (Vertex a) [Edge a] } deriving (Show)

type EdgesIn a = [Edge a]
type EdgesOut a = [Edge a]

data Edge a = MEdge (Vertex a) (Vertex a) | IEdge (Vertex a) (Vertex a) deriving (Eq, Ord, Show)

edgeSrc :: Edge a -> Vertex a
edgeSrc (MEdge v1 _) = v1
edgeSrc (IEdge v1 _) = v1

edgeDst :: Edge a -> Vertex a
edgeDst (MEdge _ v2) = v2
edgeDst (IEdge _ v2) = v2


data Graph a = Graph [Vertex a] [Edge a] (AdjList a) deriving (Show)

data WeightedGraph a = WGraph (Graph a) (EdgeWeights a) (VertexWeights a) deriving (Show)

type SCCGraph = Graph

type EdgeWeights a = M.Map (Edge a) Integer
type VertexWeights a = M.Map (Vertex a) Integer

type ExprEdge = Edge Expr
type ExprVertex = Vertex Expr

expr2String (Expr x p) = 
    if x == "" 
    then show p
    else if p /= 0
        then if p > 0
             then x ++ "+" ++ show p
             else x ++ "-" ++ show (-p)
        else x

zeroVertex = Vertex (Expr "" 0)

solveInputConstraints :: [InputConstraint] -> Either SolverFailure (M.Map Variable Integer)
solveInputConstraints = solveConstraints . convertInputConstraints


convertInputConstraints :: [InputConstraint] -> [Constraint]
convertInputConstraints inConstraints = inConstraints >>= f
  where f (LEQConstraintI e1 e2) = [LEQConstraint e1 e2]
        f (EQConstraintI e1 e2) = [LEQConstraint e1 e2, LEQConstraint e2 e1]
        f (GEQConstraintI e1 e2) = [LEQConstraint e2 e1]

constraintToEdge :: Constraint -> ExprEdge
constraintToEdge (LEQConstraint e1 e2) = MEdge (Vertex e1) (Vertex e2)

data SolverFailure = SolveFailIntraComponentEdges [Vertex Expr]
                   | SolveFailInterComponentEdges ([Vertex Expr], [Vertex Expr], [Integer])
                   | SolveFailSetImmutable
                   | SolveFailExceededMaxTime
                   | SolveFailDidntAssignVariables [Variable]
                   
solverFailureToString :: SolverFailure -> String
solverFailureToString (SolveFailIntraComponentEdges exprVertices) = "Time constraint failure: detected impossible inequality chain / equality of " ++ (intercalate " = " $ map (expr2String . (\(Vertex e) -> e)) exprVertices)

solverFailureToString (SolveFailInterComponentEdges (exprVerticesSrc, exprVerticesDst, weights)) = "Time constraint failure: detected multiple inter-component edges of differing immutable weights\nSource component vertex: " ++ intercalate " " (map (expr2String . (\(Vertex e) -> e)) exprVerticesSrc) ++ "\nDestination component vertex: " ++ intercalate " " (map (expr2String . (\(Vertex e) -> e)) exprVerticesDst) ++ "\nImmutable edges of weight: " ++ intercalate " " (map show weights)


solverFailureToString SolveFailSetImmutable = "Time constraint failure: detected change of immutable variable"

solverFailureToString SolveFailExceededMaxTime = "Time constraint failure: detected an exceeding of max vertex weight"

solverFailureToString (SolveFailDidntAssignVariables vars) = "Time constraint failure: result hadn't assigned the following variables correctly: " ++ (intercalate " " vars) ++ "\n Also this shouldn't happen."
                   
                   
solveConstraints :: [Constraint] -> Either SolverFailure (M.Map Variable Integer)
solveConstraints constraints = 
    case constraintsToGraphH constraints of
        Left (HFailIntraComponent errVals) -> Left $ SolveFailIntraComponentEdges errVals
        Left (HFailInterComponent errVals) -> Left $ SolveFailInterComponentEdges errVals
        Right graphHData -> case assignPathLengths graphHData of
                              Right (componentVertexWeights, componentEdgeWeights) ->
                                    let vars = getAllVariables . getAllExpressions $ constraints
                                        exprVertexToCompVertex = ghExprVertexToCompVertex graphHData
                                    in case extractVariableWeights componentVertexWeights vars exprVertexToCompVertex of
                                            Right varVals -> Right varVals
                                            Left notAssigned -> Left $ SolveFailDidntAssignVariables notAssigned
                              Left err -> Left err
                              
    
    
solveConstraintsAndReturnGraphAndWeights :: [Constraint] -> Either SolverFailure (GraphHData, VertexWeights Integer, EdgeWeights Integer)
solveConstraintsAndReturnGraphAndWeights constraints = 
    case constraintsToGraphH constraints of
        Left (HFailIntraComponent errVals) -> Left $ SolveFailIntraComponentEdges errVals
        Left (HFailInterComponent errVals) -> Left $ SolveFailInterComponentEdges errVals
        Right graphHData -> case assignPathLengths graphHData of
                              Right (componentVertexWeights, componentEdgeWeights) -> Right (graphHData, componentVertexWeights, componentEdgeWeights)
                              Left err -> Left err
    

extractVariableWeights :: M.Map (Vertex Integer) Integer -> [Variable] -> M.Map (Vertex Expr) (Vertex Integer) -> Either [Variable] (M.Map Variable Integer)
extractVariableWeights componentVertexWeights vars mapExprVertexComponentVertex = 
    let varsVertices = zip vars $ map Vertex $ variablesToVariableExpressions0Summand vars
        listOptVarsValues = map f varsVertices -- [Either Variable (Variable, Integer)]
        -- traverse :: (a -> f b) -> t a -> f (t b), so set (a -> f b) = id, then a = f b, so (f b -> f b) -> t (f b) -> f (t b)
        -- optListVarsValues :: Maybe [(Variable, Integer)]
        optListVarsValues = traverse id listOptVarsValues
    in case optListVarsValues of
        Left _ -> Left $ lefts listOptVarsValues                 -- the not-assigned values
        Right listVarsValues -> Right $ M.fromList listVarsValues
    
    where f (var, exprV) = let compV = mapExprVertexComponentVertex M.! exprV
                           in maybe (Left var) (\w -> Right (var, w)) $ componentVertexWeights M.!? compV
                                
                                
    
variablesToVariableExpressions0Summand :: [Variable] -> [Expr]
variablesToVariableExpressions0Summand = map (flip Expr 0)
    

constraintsToGraphH :: [Constraint] -> Either GraphHFailure GraphHData
constraintsToGraphH = buildGraphH . buildGraphG


getAllExpressions :: [Constraint] -> [Expr]
getAllExpressions = S.toList . foldr f S.empty 
    where f (LEQConstraint e1 e2) = S.insert e1 . S.insert e2

getAllVariables :: [Expr] -> [Variable]
getAllVariables = S.toList . foldr f S.empty
    where f (Expr v _) = S.insert v

ensureZeroSummands :: [Expr] -> [Expr]
ensureZeroSummands exprs = 
    let vars = getAllVariables exprs
    in S.toList $ foldr (\var -> S.insert (Expr var 0)) (S.fromList exprs) vars

buildVertices :: [Expr] -> [ExprVertex]
buildVertices exprs = foldr (\e ->  (Vertex e:)) [zeroVertex] (ensureZeroSummands exprs)
    
    
buildGraphG :: [Constraint] -> (Graph Expr, EdgeWeights Expr, VertexWeights Expr)
buildGraphG constraints = 
    let (vs, es, ews) = buildGraph2 constraints
        als = buildAdjs vs es
    in (Graph vs es als, ews, initAllVertices vs)

-- initialise all vertices to w(v)=-1 (no assignment), except 0-vertex of weight 0
initAllVertices :: [ExprVertex] -> VertexWeights Expr
initAllVertices vs = 
    let vs' = filter (/= zeroVertex) vs
        zeroMap = M.singleton zeroVertex 0
    in foldr (\v m -> M.insert v (-1) m) zeroMap vs'

-- note: adjacencies for all vertices! both incoming and outgoing! even if empty!
buildAdjs :: Ord a => [Vertex a] -> [Edge a] -> AdjList a
buildAdjs vs = foldr f $ AdjList initEmpty initEmpty
    where -- f :: Ord a => Edge a -> AdjList a -> AdjList a
          f e (AdjList inc out) = AdjList (M.insertWith (++) (edgeDst e) [e] inc) (M.insertWith (++) (edgeSrc e) [e] out)
          -- initEmpty :: Ord a => M.Map (Vertex a) [Edge a]
          initEmpty = foldr (\v m -> M.insert v [] m) M.empty vs
          
          
buildGraph2 :: [Constraint] -> ([ExprVertex], [ExprEdge], EdgeWeights Expr)
buildGraph2 cs = 
    let exprs = getAllExpressions cs
        vs = buildVertices exprs
        mes = buildMutableEdges cs vs
        (ies, ws) = buildImmutableEdges vs
    in (vs, mes ++ ies, ws)
        
        
        
buildMutableEdges :: [Constraint] -> [ExprVertex] -> [ExprEdge]
buildMutableEdges constraints vs = 
    let implicitConstraints = generateImplicitConstraints vs
    in map constraintToEdge constraints ++ implicitConstraints

    
generateImplicitConstraints :: [ExprVertex] -> [ExprEdge]
generateImplicitConstraints = map (MEdge zeroVertex) . getSmallestSummandVertexPerVariable . filter (/= zeroVertex)
    
getSmallestSummandVertexPerVariable :: [ExprVertex] -> [ExprVertex]
getSmallestSummandVertexPerVariable vs = 
    let verticesByVariable = groupBy groupfn vs
        sortedVerts = map (sortBy sortfn) verticesByVariable
    in map head $ filter (/= []) sortedVerts
    
    where groupfn :: ExprVertex -> ExprVertex -> Bool
          groupfn (Vertex (Expr v1 _)) (Vertex (Expr v2 _)) = v1 == v2
          
          sortfn :: ExprVertex -> ExprVertex -> Ordering
          sortfn (Vertex (Expr _ i1)) (Vertex (Expr _ i2)) = compare i1 i2
    
vertexSummand :: ExprVertex -> Integer
vertexSummand (Vertex (Expr _ p)) = p
    
buildImmutableEdges :: [ExprVertex] -> ([ExprEdge], EdgeWeights Expr)
buildImmutableEdges vs = 
    let vs' = filter (/= zeroVertex) vs
        verticesByVariable = groupBy groupfn vs'
        sortedVerts = map (sortBy sortfn) verticesByVariable
    in foldr1 mergefn $ map createEdges sortedVerts
    
    where groupfn :: ExprVertex -> ExprVertex -> Bool
          groupfn (Vertex (Expr v1 _)) (Vertex (Expr v2 _)) = v1 == v2
          
          sortfn :: ExprVertex -> ExprVertex -> Ordering
          sortfn (Vertex (Expr _ i1)) (Vertex (Expr _ i2)) = compare i1 i2

          mergefn :: ([ExprEdge], EdgeWeights Expr) -> ([ExprEdge], EdgeWeights Expr) -> ([ExprEdge], EdgeWeights Expr)
          mergefn (es, eim) (ess, w) = (es ++ ess, M.union eim w)
          
          createEdges :: [ExprVertex] -> ([ExprEdge], EdgeWeights Expr)
          createEdges vs | length vs < 2 = ([], M.empty)
                         | otherwise = 
                            let vertexPairs = zip vs (tail vs)
                            in foldr edgeAndWeightFromVertexPair ([], M.empty) vertexPairs
          
          edgeAndWeightFromVertexPair :: (ExprVertex, ExprVertex) -> ([ExprEdge], EdgeWeights Expr) -> ([ExprEdge], EdgeWeights Expr)
          edgeAndWeightFromVertexPair (v1, v2) (es, ws) = 
                    let e = IEdge v1 v2
                        we = (vertexSummand v2) - (vertexSummand v1)
                    in (e:es, M.insert e we ws)
        
-- we're not going to bother with the weights - they're not needed in the SCC-reversed graph
reverseGraph :: Ord a => Graph a -> Graph a
reverseGraph (Graph vs es al) = 
    let es' = map edgeRev es
        al' = buildAdjs vs es'
    in Graph vs es' al'
    where edgeRev :: Edge a -> Edge a
          edgeRev (IEdge v1 v2) = IEdge v2 v1
          edgeRev (MEdge v1 v2) = MEdge v2 v1

          
          
-- might fail!
-- also needs refactoring!

data GraphHData = GraphHData { ghGraph :: Graph Integer,
                               ghComponentZeroVertex :: Vertex Integer,
                               ghEdgeWeightsExpr :: EdgeWeights Expr,
                               ghVertexWeightsExpr :: VertexWeights Expr,
                               ghEdgeWeightsComponent :: EdgeWeights Integer,
                               ghVertexWeightsComponent :: VertexWeights Integer,
                               ghCompVertexToExprVertices :: M.Map (Vertex Integer) [Vertex Expr],
                               ghExprVertexToCompVertex :: M.Map (Vertex Expr) (Vertex Integer),
                               ghCompEdgeToExprEdges :: M.Map (Edge Integer) [Edge Expr],
                               ghExprEdgeToCompEdge :: M.Map (Edge Expr) (Edge Integer) }

data GraphHFailure = HFailIntraComponent [Vertex Expr]                               
                   | HFailInterComponent ([Vertex Expr], [Vertex Expr], [Integer]) 
                   

buildGraphH :: (Graph Expr, EdgeWeights Expr, VertexWeights Expr) -> Either GraphHFailure GraphHData
buildGraphH (g, ews, vws) = 
    let (vertex2ComponentID, componentID2Vertices) = scc g
        (Graph vs es al) = g
        componentVs = nub . map (\v -> Vertex $ vertex2ComponentID M.! v) $ vs
        componentZeroVertex = Vertex (vertex2ComponentID M.! zeroVertex)
        componentVWeights = M.singleton componentZeroVertex 0       -- zero vertex has weight 0! no other vertices have weights at this point
        compVertexToExprVertices :: M.Map (Vertex Integer) [Vertex Expr]
        compVertexToExprVertices = M.mapKeys Vertex componentID2Vertices
        
        exprVertexToCompVertex :: M.Map (Vertex Expr) (Vertex Integer)
        exprVertexToCompVertex = M.map Vertex vertex2ComponentID
        (intraComponentEdges, interComponentEdges) = categoriseEdgesIntraInterComponent . edgesByComponentID vertex2ComponentID $ es
        componentIDIntraComponentEdges = M.toList $ mapEdgesByComponentID intraComponentEdges
        componentIDInterComponentEdges = M.toList $ mapEdgesByComponentID interComponentEdges -- List $ M.Map (Integer, Integer) [Edge a]
        
        -- intra-component-vertex edges:
        optEws' = execStateT (mapM_ (\(_, intra) -> mergeIntraComponentEdges intra) componentIDIntraComponentEdges) ews
        
        
        -- inter-component-vertex edges:
        -- mapWithKey :: (k -> a -> b) -> Map k a -> Map k b
        -- mapKeys :: (k1 -> k2) -> Map k1 a -> Map k2 a
        -- probably going to need to list-convert it for the mapM_
        -- [((Integer, Integer), [Edge a])]
        -- map (\((compSrc, compDst), edges) -> (edgeComponentThunk compSrc compDst, edges))
        -- uncurry createRepresentativeInterComponentEdge
        -- then mapM_ using that
        
        
        -- also need a new set of vertices for component vertices
        -- and the mappings from one to tother
        -- and the weight(s) mapping - 0-vertex to component-vs, all vertices from component-vs
        
    in case optEws' of
        Left failVertex -> Left . HFailIntraComponent $ (compVertexToExprVertices M.!) $ exprVertexToCompVertex M.! failVertex
        Right ews' -> let optS = 
                            execStateT (mapM_ (uncurry createRepresentativeInterComponentEdge) 
                                              (map (\((componentSrc, componentDst), inter) -> (edgeComponentThunk componentSrc componentDst, inter)) componentIDInterComponentEdges)) 
                                       (M.empty, M.empty, [], ews', M.empty)
                      in case optS of
                            Right (mapEdgeAToEdgeB, mapEdgeBToEdgesA, edgesB, edgeWeightsA, edgeWeightsB) -> 
                                Right $ GraphHData (Graph componentVs edgesB (buildAdjs componentVs edgesB))
                                                  componentZeroVertex
                                                  edgeWeightsA
                                                  vws
                                                  edgeWeightsB
                                                  componentVWeights
                                                  compVertexToExprVertices
                                                  exprVertexToCompVertex
                                                  mapEdgeBToEdgesA
                                                  mapEdgeAToEdgeB
                                                  
                            Left (srcCV, dstCV, difftWeights) -> let srcExpVs = compVertexToExprVertices M.! srcCV
                                                                     dstExpVs = compVertexToExprVertices M.! dstCV
                                                                 in Left $ HFailInterComponent (srcExpVs, dstExpVs, difftWeights)
    
    
edgesByComponentID :: Ord a => M.Map (Vertex a) Integer -> [Edge a] -> [(Edge a, (Integer, Integer))]
edgesByComponentID vertex2ComponentID es = zip es . map f $ es
    where f e = (vertex2ComponentID M.! edgeSrc e, vertex2ComponentID M.! edgeDst e)

-- fst: the edges where the components are the same, snd: inter-component edges
categoriseEdgesIntraInterComponent :: Ord a => [(Edge a, (Integer, Integer))] -> ([(Edge a, (Integer, Integer))], [(Edge a, (Integer, Integer))])
categoriseEdgesIntraInterComponent es = 
    (filter f es, filter (not . f) es)
    where f (_, (src, dst)) = src == dst
    
    
mapEdgesByComponentID :: Ord a => [(Edge a, (Integer, Integer))] -> M.Map (Integer, Integer) [Edge a]
mapEdgesByComponentID = foldr (\(e, (i1, i2)) m -> M.insertWith (++) (i1, i2) [e] m) M.empty 

-- we know that all immutable edges are w > 0, so any immutable edge leads to failure
-- otherwise, set all mutable edges going "between" this component to weight 0
mergeIntraComponentEdges :: Ord a => [Edge a] -> StateT (EdgeWeights a) (Either (Vertex a)) ()
mergeIntraComponentEdges es = 
    if any isImmutable es
    then lift $ Left $ edgeSrc $ head es
    else do
        ewa <- get
        let ewa' = foldr (\e m -> M.insert e 0 m) ewa es
        put ewa'


isImmutable :: Edge a -> Bool
isImmutable (IEdge _ _) = True
isImmutable (MEdge _ _) = False

isMutable = not . isImmutable


-- takes a source component integer id, a dest component integer id, and returns a function
-- that when given True will create a mutable edge, and False an immutable edge
edgeComponentThunk :: Integer -> Integer -> (Bool -> Edge Integer)
edgeComponentThunk componentSrc componentDst =
    createEdgeFn (\() -> MEdge (Vertex componentSrc) (Vertex componentDst))
                 (\() -> IEdge (Vertex componentSrc) (Vertex componentDst))

createEdgeFn :: (() -> Edge b) -> (() -> Edge b) -> Bool -> Edge b
createEdgeFn createMutable createImmutable = 
    \b -> if b 
          then createMutable ()
          else createImmutable ()


-- Making a decision here: Component-representing vertices are Vertex i, where i is the component number
-- Function is given True to produce a mutable edge, False to produce an immutable edge
-- we assume that es /= []
-- returns updated edge weights
-- could prooooobably be refactored!
type InterComponentEdgeState a b = (M.Map (Edge a) (Edge b), M.Map (Edge b) [Edge a], [Edge b], EdgeWeights a, EdgeWeights b)

-- mapEdgeAToEdgeB, mapEdgeBToEdgesA, edgesB, edgeWeightsA, edgeWeightsB

type InterComponentSt a b = StateT (InterComponentEdgeState a b) (Either (Vertex b, Vertex b, [Integer]))  ()

createRepresentativeInterComponentEdge :: (Ord a, Ord b) => (Bool -> Edge b) -> [Edge a] -> InterComponentSt a b
createRepresentativeInterComponentEdge edgeGen [] = error "createRepresentativeInterComponentEdge given an empty edge list"
createRepresentativeInterComponentEdge edgeGen es = do
    (mapEdgeAToEdgeB, mapEdgeBToEdgesA, edgesB, ewa, ewb) <- get
    case getEdgePlusWeight ewa of
        Left difftWeights -> let vertexSrc = edgeSrc $ edgeGen True         -- doesn't matter which bool i use
                                 vertexDst = edgeDst $ edgeGen True
                             in lift $ Left $ (vertexSrc, vertexDst, difftWeights)
        Right (edgeB, optWeight) -> do
            let mapEdgeAToEdgeB' = M.union mapEdgeAToEdgeB $ M.fromList $ zip es (repeat edgeB)
                mapEdgeBToEdgesA' = M.insert edgeB es mapEdgeBToEdgesA
                edgesB' = edgeB:edgesB
                
            case optWeight of
                Nothing -> put (mapEdgeAToEdgeB', mapEdgeBToEdgesA', edgesB', ewa, ewb)
                Just immW -> do
                    let weightsEdgesA' = foldr (\e m -> M.insert e immW m) ewa es -- we're overwriting immutables; doesn't matter - they're the same weight anyway!
                        weightsEdgesB' = M.insert edgeB immW ewb
                    put (mapEdgeAToEdgeB', mapEdgeBToEdgesA', edgesB', weightsEdgesA', weightsEdgesB')
            
    
    where checkSameImmutableWeights ews = 
            let weights = map (\e -> ews M.! e) $ filter isImmutable es
                weightsNub = nub weights
                difftWeights = length (nub weights) /= 1
            in if difftWeights then Left weightsNub else Right $ head weightsNub   -- not precisely one immutable weight! also this is called only if at least one immutable weight exists
          
          -- getEdgePlusWeight :: EdgeWeights a -> Maybe (Edge b, Maybe Integer)
          -- Just (edgeB, Just Integer) if there are immutable edges and succeeds
          -- Just (edgeB, Nothing) if there are no immutable edges
          -- Nothing if there are immutables of different weight
          getEdgePlusWeight ews = let allMutable = all isMutable es
                                      edgeB = edgeGen allMutable
                                  in if allMutable
                                     then Right (edgeB, Nothing)
                                     else fmap (\immW -> (edgeB, Just immW)) $ checkSameImmutableWeights ews

    
scc :: Ord a => Graph a -> (M.Map (Vertex a) Integer, M.Map Integer [Vertex a])
scc g = 
    let vertex2Component = 
            flip evalState ([], S.empty) $ do
                sccFirstDFS
                modify (\(stk, _) -> (stk, S.empty))  -- preserve the stack that gives the ordering, reset the visited
                sccSecondDFS
        component2Vertex =  foldr (\(v, i) m -> M.insertWith (++) i [v] m) M.empty $ M.toList vertex2Component
    in (vertex2Component, component2Vertex)
        
    where (Graph vs es (AdjList _ outG)) = g
          grev = reverseGraph g
          (Graph _ _ (AdjList _ outGRev)) = grev
          -- from here
          -- https://www.hackerearth.com/practice/algorithms/graphs/strongly-connected-components/tutorial/
          --sccFirstDFS :: Ord a => State ([Vertex a], S.Set (Vertex a)) ()
          sccFirstDFS = mapM_ sccDFS1 vs -- we can do this because of the visited-check at the start
          -- sccDFS1 :: Vertex a -> State ([Vertex a], S.Set (Vertex a)) ()
          sccDFS1 u = do
                (_, visited) <- get
                if S.member u visited
                then return ()                   -- we really need to do this in the middle of the mapM_...
                else do
                    modify (\(stk, visited) -> (stk, S.insert u visited))
                    mapM_ (sccDFS1 . edgeDst) (outG M.! u)
                    modify (\(stk, visited) -> (u:stk,visited))
                
          --sccSecondDFS :: Ord a => State ([Vertex a], S.Set (Vertex a)) (M.Map (Vertex a) Integer)
          sccSecondDFS = do
                (stk, visited) <- get
                let (_, _, mapping, _) = flip execState (stk, visited, M.empty, -1) $ mapM_ sccDFS2' stk
                return mapping
                
          
          -- this splits off the increment of the component
          --sccDFS2' :: Ord a => Vertex a -> State ([Vertex a], S.Set (Vertex a), M.Map (Vertex a) Integer, Integer) ()
          sccDFS2' u = do
                (_, visited, _, _) <- get
                if S.member u visited
                then return () 
                else do
                    modify (\(stk, visited, mapping, component) -> (stk, visited, mapping, component + 1))
                    sccDFS2 u
                
                
          
          -- State is stack, visited, vertex components, current component
          -- sccDFS2 :: Ord a => Vertex a -> State ([Vertex a], S.Set (Vertex a), M.Map (Vertex a) Integer, Integer) ()
          sccDFS2 u = do
                (_, visited, _, _) <- get
                if S.member u visited 
                then return ()                   -- we really need to do this in the middle of the mapM_...
                else do 
                    modify (\(stk, visited, mapping, component) -> (stk, S.insert u visited, mapping, component))
                    mapM_ (sccDFS2 . edgeDst) (outGRev M.! u)
                    modify (\(stk, visited, mapping, component) -> (u:stk,visited, M.insert u component mapping, component))
          

    
{- 
    ****************************************************
        ASSIGN PATH LENGTHS MAIN LOOP
    ****************************************************
-}
          

assignPathLengths :: GraphHData -> Either SolverFailure (VertexWeights Integer, EdgeWeights Integer)
assignPathLengths ghd = 
    let immutableVertices = S.singleton (ghComponentZeroVertex ghd)
        totalImmutableSum = M.foldr (+) 0 (ghEdgeWeightsComponent ghd)
        vws = ghVertexWeightsComponent ghd
        ews = ghEdgeWeightsComponent ghd
        (Graph _ _ adjListMap) = ghGraph ghd
        initialMainLoopState = MLS vws
                                   ews
                                   adjListMap
                                   immutableVertices 
                                   totalImmutableSum 
                                   immutableVertices
    in fmap f $ execStateT assignPathLengths' initialMainLoopState
    where f mls = (mlsVWs mls, mlsEWs mls)
    

assignPathLengths' :: StateT MainLoopState (Either SolverFailure) ()
assignPathLengths' = do
    terminate <- isEmptyForChange
    if terminate
    then return ()
    else do
        forChangeForwardL <- fmap S.toList $ getForChange
        forChangeBackwardL <- fmap unionAllL $ mapM forwardPropagate forChangeForwardL
        forChangeForwardNext <- fmap unionAll $ mapM backwardPropagate forChangeBackwardL
        setForChange forChangeForwardNext
        assignPathLengths' -- lmao would have been good to remember this!!! turns out iteration requires a call. hmm. "would you like fries with that". "WOULD you like FRIES with that?". getting better, soon i'll be able to get a job
    
  where unionAll :: [S.Set (Vertex Integer)] -> S.Set (Vertex Integer)
        unionAll = foldr S.union S.empty
  
        unionAllL :: [S.Set (Vertex Integer)] -> [Vertex Integer]
        unionAllL = S.toList . foldr S.union S.empty
    
{- 
    new state required: set inProgress, stack s, set changeBackward
    main state changes: vertex weights, edge weights
    may fail
-}

-- (vws, ews, adjListMap, immutableV, totalImmutableEdgeWeight, toChange)
data MainLoopState = MLS { mlsVWs :: VertexWeights Integer, 
                           mlsEWs :: EdgeWeights Integer, 
                           mlsALs :: AdjList Integer, 
                           mlsImmutables :: S.Set (Vertex Integer),
                           mlsTotalImmutableWeight :: Integer,
                           mlsForChange :: S.Set (Vertex Integer) }

-- (mls, inProgress, s, changeForward/Backward)
-- ((vws, ews, adjListMap, immutableV, totalImmutableEdgeWeight), inProgress, s, change)
data InnerLoopState = ILS { ilsMLS :: MainLoopState, 
                            ilsInProgress :: S.Set (Vertex Integer), 
                            ilsStack :: [Vertex Integer], 
                            ilsToChange :: S.Set (Vertex Integer) }


{- 
    ****************************************************
        ALGORITHM UTILITY
    ****************************************************
-}
-- MAIN LOOP
resetForChange :: StateT MainLoopState (Either SolverFailure) ()
resetForChange = modify (\mls -> mls { mlsForChange = S.empty }) -- (vws, ews, adjListMap, immutableV, totalImmutableEdgeWeight, S.empty))
    
isEmptyForChange :: StateT MainLoopState (Either SolverFailure) Bool
isEmptyForChange = do
    forChange <- gets mlsForChange
    return $ S.null forChange 

getForChange :: StateT MainLoopState (Either SolverFailure) (S.Set (Vertex Integer))
getForChange = gets mlsForChange
    
setForChange :: S.Set (Vertex Integer) -> StateT MainLoopState (Either SolverFailure) ()
setForChange newForChange = modify (\mls -> mls { mlsForChange = newForChange } )
    
-- SECONDARY LOOPS
-- (\(mls, inProgress, s, changeForward) -> (mls, inProgress, v:s, changeForward))
push :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) ()
push v = modify (\ils -> ils { ilsStack = v:ilsStack ils })

pop :: StateT InnerLoopState (Either SolverFailure) ()
pop = modify (\ils -> ils { ilsStack = (tail . ilsStack) ils })

-- will except if nothing on stack
peek :: StateT InnerLoopState (Either SolverFailure) (Vertex Integer)
peek = do
    us <- gets ilsStack
    return $ head us
    
isStackEmpty :: StateT InnerLoopState (Either SolverFailure) Bool
isStackEmpty = do
    s <- gets ilsStack
    return $ s == []
    
-- (\(mls, inProgress, s, change) -> (mls, inProgress, s, S.insert v change))
addToChange :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) ()
addToChange v = modify (\ils -> ils { ilsToChange = S.insert v (ilsToChange ils) })
    

isInProgress :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) Bool
isInProgress v = do
    inProgress <- gets ilsInProgress
    return $ S.member v inProgress
    
-- (\(mls, inProgress, s, change) -> (mls, S.insert v inProgress, s, change))
addInProgress :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) ()
addInProgress v = modify (\ils -> ils { ilsInProgress = S.insert v (ilsInProgress ils) })
    

-- (\(mls, inProgress, s, change) -> (mls, S.delete v inProgress, s, change))
removeInProgress :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) ()
removeInProgress v = modify (\ils -> ils { ilsInProgress = S.delete v (ilsInProgress ils) })
    
    
isImmutableV :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) Bool
isImmutableV v = do
    imms <- gets (mlsImmutables . ilsMLS)
    return $ S.member v imms
    

wv :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) (Maybe Integer)
wv u = do
    vws <- gets (mlsVWs . ilsMLS)
    return $ vws M.!? u

we :: Edge Integer -> StateT InnerLoopState (Either SolverFailure) (Maybe Integer)
we e = do
    ews <- gets (mlsEWs . ilsMLS)
    return $ ews M.!? e

-- (\((vws, ews, adjListMap, immutableV, totalImmutableEdgeWeight, changesMain), inProgress, s, change) -> ((M.insert v w vws, ews, adjListMap, immutableV, totalImmutableEdgeWeight, changesMain), inProgress, s, change))
setWV :: Vertex Integer -> Integer -> StateT InnerLoopState (Either SolverFailure) ()
setWV v w = modify (\ils -> let mls = ilsMLS ils 
                            in ils { ilsMLS = mls { mlsVWs = M.insert v w (mlsVWs mls) } } )
    

-- (\((vws, ews, adjList, immutableV, totalImmutableEdgeWeight, changesMain), inProgress, s, change) -> ((vws, M.insert e w ews, adjList, immutableV, totalImmutableEdgeWeight, changesMain), inProgress, s, change))
setWE :: Edge Integer -> Integer -> StateT InnerLoopState (Either SolverFailure) ()
setWE e w = modify (\ils -> let mls = ilsMLS ils 
                            in ils { ilsMLS = mls { mlsEWs = M.insert e w (mlsEWs mls) } } )

    
getOutgoings :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) [Edge Integer]
getOutgoings u = do
    adjList <- gets (mlsALs . ilsMLS)
    return $ outgoing adjList M.! u


getIncomings :: Vertex Integer -> StateT InnerLoopState (Either SolverFailure) [Edge Integer]
getIncomings v = do
    adjList <- gets (mlsALs . ilsMLS)
    return $ incoming adjList M.! v

getImmutableSum :: StateT InnerLoopState (Either SolverFailure) Integer
getImmutableSum = gets (mlsTotalImmutableWeight . ilsMLS)
    
    
{- 
   ****************************************************
        FORWARD PROPAGATE
   ****************************************************
-}

-- return value is changeBackwards; we're discarding inProgress and changeForward
-- also propagate the changed main loop state
forwardPropagate :: Vertex Integer -> StateT MainLoopState (Either SolverFailure) (S.Set (Vertex Integer))
forwardPropagate from = do
    mls <- get
    case execStateT forwardPropagate' (ILS mls S.empty [from] S.empty) of
        Right (ILS mls inProgress s change) -> do
                        put mls
                        return change
        Left err -> lift $ Left err

    
forwardPropagate' :: StateT InnerLoopState (Either SolverFailure) ()
forwardPropagate' = do
    emptyStack <- isStackEmpty
    if emptyStack
    then return ()
    else do
        u <- peek
        terminateBranch <- isInProgress u
        if terminateBranch
        then do
            removeInProgress u
            pop
            forwardPropagate'
        else do
            addInProgress u
            uvEdges <- getOutgoings u
            mapM_ processEdgeForward uvEdges
            forwardPropagate'
        
processEdgeForward :: Edge Integer -> StateT InnerLoopState (Either SolverFailure) ()
processEdgeForward e@(MEdge u v) = do
    wuOpt <- wv u
    wvOpt <- wv v
    case (wuOpt, wvOpt) of
        (Nothing, _) -> error "Tried forward traversing edges u->v with unassigned weight of u" -- unassigned u and we're still here??
        
        (Just uWeight, Nothing) -> do
            setWE e 0
            setWV v uWeight
            addToChange v
            push v
            
        (Just uWeight, Just vWeight) ->
            if vWeight - uWeight >= 0
            then do 
                uvWeightOpt <- we e
                case uvWeightOpt of
                    Nothing -> setWE e $ vWeight - uWeight -- removed an exception here; makes sense - might have u and v assigned via different paths that didn't cover u->v
                    Just uvWeight -> 
                        if vWeight - uWeight /= uvWeight
                        then setWE e $ vWeight - uWeight
                        else return ()
                 
                
            else do -- vWeight - uWeight < 0
                immutableV <- isImmutableV v
                if immutableV
                then lift $ Left $ SolveFailSetImmutable
                else do
                    setWE e 0
                    setWV v uWeight
                    addToChange v
                    push v
                 
        
processEdgeForward e@(IEdge u v) = do
    wuOpt <- wv u
    wvOpt <- wv v
    wuvOpt <- we e
    immSum <- getImmutableSum
    case (wuOpt, wvOpt, wuvOpt) of
        (Nothing, _, _) -> error "Vertex u weight didn't exist"
        
        (Just uWeight, Nothing, Just uvWeight) ->
            if uWeight + uvWeight > immSum
            then lift $ Left $ SolveFailExceededMaxTime
            else do
                setWV v $ uWeight + uvWeight
                addToChange v
                push v
        
        (Just uWeight, Just vWeight, Just uvWeight) ->
            if uWeight + uvWeight > immSum
            then lift $ Left $ SolveFailExceededMaxTime
            else do
                immutableV <- isImmutableV v
                if immutableV
                then if uWeight + uvWeight /= vWeight
                     then lift $ Left $ SolveFailSetImmutable
                     else return ()
                else if uWeight + uvWeight < vWeight
                     then addToChange v
                     else if uWeight + uvWeight > vWeight
                     then do
                        setWV v $ uWeight + uvWeight
                        addToChange v
                        push v
                     else return ()
                          
                
    
{-
   ****************************************************
        BACKWARD PROPAGATE
   ****************************************************
-}
backwardPropagate :: Vertex Integer -> StateT MainLoopState (Either SolverFailure) (S.Set (Vertex Integer))
backwardPropagate from = do
    mls <- get
    case execStateT backwardPropagate' (ILS mls S.empty [from] S.empty) of
        Right (ILS mls inProgress s change) -> do
                        put mls
                        return change
        Left err -> lift $ Left err

    
backwardPropagate' :: StateT InnerLoopState (Either SolverFailure) ()
backwardPropagate'  = do
    emptyStack <- isStackEmpty
    if emptyStack
    then return ()
    else do
        v <- peek
        terminateBranch <- isInProgress v
        if terminateBranch
        then do
            removeInProgress v
            pop
            backwardPropagate'
        else do
            addInProgress v
            uvEdges <- getIncomings v
            mapM_ processEdgeBackward uvEdges
            backwardPropagate'
        
processEdgeBackward :: Edge Integer -> StateT InnerLoopState (Either SolverFailure) ()
processEdgeBackward e@(MEdge u v) = do
    wuOpt <- wv u
    wvOpt <- wv v
    case (wuOpt, wvOpt) of
        (Nothing, _) -> error "weight u not assigned"
        (_, Nothing) -> error "weight v not assigned"
        (Just uWeight, Just vWeight) -> 
            if uWeight < vWeight
            then setWE e $ vWeight - uWeight
            else if uWeight > vWeight
            then addToChange u
            else return ()


processEdgeBackward e@(IEdge u v) = do
    wuOpt <- wv u
    wvOpt <- wv v
    wuvOpt <- we e
    immSum <- getImmutableSum
    immutableV <- isImmutableV u
    case (wuOpt, wvOpt, wuvOpt) of
        (Nothing, _, _) -> error "weight u not assigned"
        (_, Nothing, _) -> error "weight v not assigned"
        (Just uWeight, Just vWeight, Just uvWeight) -> do
            if vWeight - uvWeight < 0
            then error "negative assignment to vertex"
            else if vWeight - uvWeight > immSum
            then error "exceeded immutable sum in an impossible way"
            else
                if uWeight < vWeight - uvWeight
                then 
                    if immutableV
                    then lift $ Left $ SolveFailSetImmutable
                    else do
                        setWV u $ vWeight - uvWeight
                        addToChange u
                        push u
                else if uWeight > vWeight - uvWeight
                     then addToChange u
                     else return ()
                
            
            
            