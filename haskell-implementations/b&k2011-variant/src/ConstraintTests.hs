module ConstraintTests where

import ConstraintSolve
import qualified Data.Map.Strict as M
import Data.Map.Merge.Strict
import Data.List
import Control.Monad
import System.IO.Unsafe
import Control.DeepSeq


-- we use DeepSeq to deepseq in fileToConstraintAndExpectedResult to force read
-- and close of a file - otherwise we get failure from too many test files open at once

{-
file format for test files:

<TEST NAME>                 line 1
#CONSTRAINTS#               marks block of constraints
x1,1<x2,2                   corresponds to x1 + 1 <= x2,2
x1,1=x2,2                   corresponds to x1 + 1 = x2 + 2      <=> x1 + 1 <= x2 + 2, x2 + 2 <= x1 + 1
.
.
.
#SOLUTION#                  marks block of solution variable values
x1,5                        corresponds to x1 = 5
x2,7                                       x2 = 7
.
.
.



-}

{-
TEST FUNCTIONS:
testBatchF -- test multiple files
testF -- test one file
runF -- run test from file, and print result on screen
runOutputF -- run test from file and print result on screen, intermediate and final graph outputs to files

testBatchF -- test multiple constraint lists
testF -- test one constraint list
runF -- run test from constraint list, and print result on screen
runOutputF -- run test from constraint list and print result on screen, intermediate and final graph outputs to files
-}

type TestName = String
type VariableMapping = M.Map Variable Integer
data TestResult = TestResult { trExpected :: Maybe VariableMapping,
                               trActual :: Either SolverFailure VariableMapping }

data BKTestErr = ExpectedSucceededActualFailed
               | ExpectedFailedActualSucceeded
               | Difference (M.Map Variable (Maybe Integer, Maybe Integer))
               | DidntAssignAllVariables [Variable]
               
data BKTestSuccess = SucceededNoDifference
                   | FailedWith SolverFailure
               
-- returns Nothing if no difference between expected and result
-- Just error otherwise
computeDifferences :: TestResult -> Either BKTestErr BKTestSuccess
computeDifferences (TestResult expectedOpt actualEither) = 
    case (expectedOpt, actualEither) of
        (Nothing, Left (SolveFailDidntAssignVariables vs)) -> Left $ DidntAssignAllVariables vs
        (Nothing, Left err) -> Right $ FailedWith err
        (Nothing, Right _) -> Left $ ExpectedFailedActualSucceeded
        (Just _, Left _) -> Left $ ExpectedSucceededActualFailed
        (Just exp, Right act) -> 
            let merged =   merge (mapMissing (\_ a -> (Just a, Nothing)))
                                 (mapMissing (\_ b -> (Nothing, Just b)))
                                 (zipWithMatched (\_ a b -> (Just a, Just b)))
                                 exp
                                 act
                allMismatches = M.filter f merged
                
            in if allMismatches == M.empty
               then Right SucceededNoDifference
               else Left . Difference $ allMismatches
               
    where f (Just _, Nothing)  = True
          f (Nothing, Just _)  = True
          f (Just a, Just b)   = a /= b
          f (Nothing, Nothing) = error "this can't happen"

                               
{-
    TESTING FILES
-}
-- test multiple files
testBatchF :: String -> IO ()
testBatchF batchName = testListFileToTestFileNames ("testsgen/" ++ batchName ++ "/index.t") >>= testBatchFN

testBatchFN :: [String] -> IO ()
testBatchFN fileNames = do
    tests <- mapM fileToConstraintAndExpectedResult fileNames
    testBatchC tests


-- test one file
testF :: String -> IO Bool
testF fileName = do
    test <- fileToConstraintAndExpectedResult fileName
    testC test


-- run and print on screen
testPF :: String -> IO ()
testPF fileName = do
    test <- fileToConstraintAndExpectedResult fileName
    testPC test

-- run and print result on screen, intermediate outputs to files
testOutF :: String -> IO ()
testOutF fileName = do
    test <- fileToConstraintAndExpectedResult fileName
    testOutC test


{-
    TESTING CONSTRAINTS
-}
testBatchC :: [(TestName, [Constraint], Maybe VariableMapping)] -> IO ()
testBatchC tests = do
    putStrLn "\nStarting tests"
    putStrLn "--------------"
    results <- mapM testC tests
    let testCt = length tests
        successes = sum . map (\b -> if True then 1 else 0) $ results
        failures = sum . map (\b -> if False then 1 else 0) $ results
    putStrLn "--------------"
    putStr $ "Tests complete: " ++ show testCt ++ "\n"
    putStrLn $ "Succeeded: " ++ show successes
    when (failures > 0) $ putStrLn $ "FAILED: " ++ show failures
    putStrLn ""
    
    
testC :: (TestName, [Constraint], Maybe VariableMapping) -> IO Bool
testC (name, constraints, expected) = do
    putStr $ name ++ ": "
    let actual = solveConstraints constraints
    case computeDifferences (TestResult expected actual) of
        Right _ -> putStrLn "succeeded" >> return True
        Left testErr -> printTestErrShort testErr >> return False
    
printTestErrShort :: BKTestErr -> IO ()
printTestErrShort ExpectedSucceededActualFailed = putStrLn "FAILED: expected success, but no solution found"
printTestErrShort ExpectedFailedActualSucceeded = putStrLn "FAILED: expected failure, but succeeded"
printTestErrShort (Difference diffMap) = do
    let diffMapL = M.toList diffMap
        cat1 = length $ filter expectedButNoActual diffMapL
        cat2 = length $ filter notExpectedButActual diffMapL
        cat3 = length $ filter different diffMapL
    putStr "FAILED: " 
    when (cat1 > 0) $ putStr $ show cat1 ++ " expected but not found; "
    when (cat2 > 0) $ putStr $ show cat2 ++ " not expected but found; "
    when (cat3 > 0) $ putStr $ show cat3 ++ " differences"
    putStr "\n"
    
  where expectedButNoActual (_, (_, Nothing)) = True
        expectedButNoActual (_, (_, _)) = False
        
        notExpectedButActual (_, (Nothing, _))= True
        notExpectedButActual (_, (_, _)) = False
        
        different (_, (Just v1, Just v2)) = v1 /= v2 -- this should already occur by default, but...
        different (_, (_, _)) = False
  
  
  
printSuccessfulResult :: M.Map Variable Integer -> IO ()
printSuccessfulResult result = do
    putStrLn "Successful. Results"
    let resultSorted = sortBy (\(var1, _) (var2, _) -> compare var1 var2) $ M.toList result
        resultStrs = map (\(var, val) -> var ++ " = " ++ show val) resultSorted
    mapM_ putStrLn resultStrs
    putStrLn "Success."
    
    
    
printSuccessfulFailure :: SolverFailure -> IO ()
printSuccessfulFailure (SolveFailIntraComponentEdges exprVertices) = do
    putStrLn "Successfully detected impossible inequality chain / equality of"
    putStrLn $ intercalate " = " $ map (expr2String . (\(Vertex e) -> e)) exprVertices
    putStrLn "Success."

printSuccessfulFailure (SolveFailInterComponentEdges (exprVerticesSrc, exprVerticesDst, weights)) = do
    putStrLn "Successfully detected multiple inter-component edges of differing immutable weights"
    putStrLn $ "Source component vertex: " ++ intercalate " " (map (expr2String . (\(Vertex e) -> e)) exprVerticesSrc)
    putStrLn $ "Destination component vertex: " ++ intercalate " " (map (expr2String . (\(Vertex e) -> e)) exprVerticesDst)
    putStrLn $ "Immutable edges of weight: " ++ intercalate " " (map show weights)
    putStrLn "Success."


printSuccessfulFailure SolveFailSetImmutable = putStrLn "Successfully detected change of immutable variable" >> putStrLn "Success."

printSuccessfulFailure SolveFailExceededMaxTime = putStrLn "Successfully detected an exceeding of max vertex weight" >> putStrLn "Success."

-- TODO: refactor the error types so we don't need this
printSuccessfulFailure (SolveFailDidntAssignVariables _) = error "Error: successful failure print function got a result that hadn't assigned its variables"



    
printFailedResultDiffs :: BKTestErr -> IO ()
printFailedResultDiffs (Difference diffMap) = do
    putStrLn "FAILED RESULT DIFFERENCES:"
    putStrLn "Var\tExp\tAct"
    let resultSorted = sortBy (\(var1, _) (var2, _) -> compare var1 var2) $ M.toList diffMap
        resultStrs = map (\(var, (valExp, valAct)) -> show var ++ "\t" ++ show valExp ++ "\t" ++ show valAct) resultSorted
    mapM_ putStrLn resultStrs
    putStrLn "TEST FAILURE!"
    
printFailedResultDiffs ExpectedSucceededActualFailed = putStrLn "FAILED: expected success, but no solution found" >> putStrLn "TEST FAILURE!"

printFailedResultDiffs ExpectedFailedActualSucceeded = putStrLn "FAILED: expected failure, but succeeded" >> putStrLn "TEST FAILURE!"
    
printFailedResultDiffs (DidntAssignAllVariables vs) = printFailedVarAssign vs >> putStrLn "TEST FAILURE!"    

printFailedVarAssign :: [Variable] -> IO ()
printFailedVarAssign varsUnassigned = putStrLn $ "FAILED: didn't assign the variables " ++ intercalate " " varsUnassigned
    
    


-- compares against solution in file
testPC :: (TestName, [Constraint], Maybe VariableMapping) -> IO ()
testPC (name, constraints, expected) = do
    putStr $ "\n" ++ name ++ ": "
    let actual = solveConstraints constraints
    case computeDifferences (TestResult expected actual) of
        Right SucceededNoDifference -> printSuccessfulResult $ (\(Right v) -> v) actual         -- it's successful so we know this exists!
        Right (FailedWith err) -> printSuccessfulFailure err
        Left testErr -> printFailedResultDiffs testErr
    putStrLn ""
    

-- does not compare against solution in file
runPC :: (TestName, [Constraint]) -> IO ()
runPC (name, constraints) = do
    putStr $ "\n" ++ name ++ ": "
    case solveConstraints constraints of
        Left (SolveFailDidntAssignVariables varsUnassigned) -> printFailedVarAssign varsUnassigned
        Left err -> printSuccessfulFailure err
        Right actual -> printSuccessfulResult actual
    putStrLn ""
    
testOutC :: (TestName, [Constraint], Maybe VariableMapping) -> IO ()
testOutC = undefined

{-
    UTILITY
-}

-- VariableMapping is expected result
fileToConstraintAndExpectedResult :: String -> IO (TestName, [Constraint], Maybe VariableMapping)
fileToConstraintAndExpectedResult fileName = do
    ls <- lines <$> readFile fileName
    deepseq ls $ return ()
    let name = head ls
        l2On = tail ls  -- skip the name!
        (constraintsSection, solutionSection) = break (== "#SOLUTION#") l2On
        constraints = convertInputConstraints . map string2ConstraintI . tail $ constraintsSection
        solution = string2Solution $ tail solutionSection
    return (name, constraints, solution)
    
  where string2ConstraintI s = 
            if '<' `elem` s
            then let (e1, _:e2) = break (== '<') s
                 in LEQConstraintI (string2Expr e1) (string2Expr e2)
            else if '=' `elem` s
            then let (e1, _:e2) = break (== '=') s
                 in EQConstraintI (string2Expr e1) (string2Expr e2)
            else error $ "constraint parse failed: no = or < on constraint " ++ s
                 
        string2Expr s = 
            let (var, _:summand) = break (== ',') s
                intSummand = read summand
            in Expr var intSummand

        string2Solution [] = Nothing
        string2Solution ss = 
            let assigns = map string2ExpectedVarAssign ss
            in Just $ M.fromList assigns
        
        string2ExpectedVarAssign s = 
            let (var, _:assigned) = break (== ',') s
                intAssigned = read assigned :: Integer
            in (var, intAssigned)
            
-- reads a file containing a list of test file names, returns those file names
testListFileToTestFileNames :: String -> IO [String]
testListFileToTestFileNames testListFileName = lines <$> readFile testListFileName



exprGraphGToFile :: String -> (Graph Expr, EdgeWeights Expr, VertexWeights Expr) -> IO ()
exprGraphGToFile fileName ((Graph vs es _), ews, vws) = do
    let vNames = M.fromList $ map vNameFn vs
        vWeightOnly = M.map (\i -> "[" ++ show i ++ "]") vws
        vLabels = merge preserveMissing
                        preserveMissing
                        (zipWithMatched (\k x y -> x ++ "\\n" ++ y))
                        vNames
                        (M.singleton zeroVertex "[0]") -- vWeightOnly -- hiding -1s for prettiness
        eLabels = M.map (\i -> show i) ews
    graphLabelsToFile fileName es vLabels eLabels
  where vNameFn v@(Vertex ex) = (v, expr2String ex)
            
            
componentGraphHToFile :: String -> GraphHData -> IO ()
componentGraphHToFile fileName ghd = do
    let (Graph _ es _) = ghGraph ghd
        ews = ghEdgeWeightsComponent ghd
        vws = ghVertexWeightsComponent ghd
        compV2ExprV = ghCompVertexToExprVertices ghd
        vNames = M.map vNameFn compV2ExprV
        vWeightOnly = M.map (\i -> "[" ++ show i ++ "]") vws
        vLabels = merge preserveMissing
                        preserveMissing
                        (zipWithMatched (\k x y -> x ++ "\\n" ++ y))
                        vNames
                        (M.singleton (ghComponentZeroVertex ghd) "[0]") -- vWeightOnly -- hiding -1s for prettiness
        eLabels = M.map (\i -> show i) ews -- since all here with weights are immutable edges
    graphLabelsToFile fileName es vLabels eLabels
  where vNameFn exprVertices = intercalate "\\n"  $ map (expr2String . (\(Vertex e) -> e)) exprVertices
        
        
solvedGraphHToFile :: String -> GraphHData -> VertexWeights Integer -> EdgeWeights Integer -> IO ()
solvedGraphHToFile fileName ghd vws ews = do
    let (Graph _ es _) = ghGraph ghd
        -- ews = ghEdgeWeightsComponent ghd
        -- vws = ghVertexWeightsComponent ghd
        compV2ExprV = ghCompVertexToExprVertices ghd
        vNames = M.map vNameFn compV2ExprV
        vWeightOnly = M.map (\i -> "[" ++ show i ++ "]") vws
        vLabels = merge preserveMissing
                        preserveMissing
                        (zipWithMatched (\k x y -> x ++ "\\n" ++ y))
                        vNames
                        vWeightOnly
        eLabels = M.mapWithKey edgeLabels ews
    graphLabelsToFile fileName es vLabels eLabels

  where vNameFn exprVertices = intercalate "\\n"  $ map (expr2String . (\(Vertex e) -> e)) exprVertices
        
        edgeLabels (MEdge _ _) i = "(" ++ show i ++ ")"
        edgeLabels (IEdge _ _) i = show i
        
graphLabelsToFile :: Ord a => String -> [Edge a] -> M.Map (Vertex a) String -> M.Map (Edge a) String -> IO ()
graphLabelsToFile fileName edges vLabels eLabels = do
    text <- buildGraphViz edges vLabels eLabels
    let noPercents = map (\c -> if c == '%' then '#' else c) text
    writeFile fileName noPercents
    

    
-- we need edges too - no guarantee that an edge will have a label
buildGraphViz :: Ord a => [Edge a] -> M.Map (Vertex a) String -> M.Map (Edge a) String -> IO String
buildGraphViz edges vLabels eLabels =
    let initial = "digraph {\n"
        verticesS = (intercalate "\n" $ map (("\""++) . (++"\"") . snd) $ M.toList vLabels) ++ "\n"
        edgesS = (intercalate "\n" $ map edgeString edges) ++ "\n"
        final = "}"
    in return $ concat [initial, verticesS, edgesS, final]
    
    where edgeString e = 
            let (vl1, vl2) = (vLabels M.! edgeSrc e, vLabels M.! edgeDst e)
                edge = "\"" ++ vl1 ++ "\"->\"" ++ vl2 ++ "\""
            in case eLabels M.!? e of
                 Nothing -> edge
                 Just edgeLabel -> edge ++ " [label=\"" ++ edgeLabel ++ "\"]"
       
            
            
-- write a time constraint set to file in the correct format with .time, human-readable with .htime
writeTimeConstraintsToFile :: String -> String -> [Constraint] -> IO ()
writeTimeConstraintsToFile fileName constraintsName constraints = do
    let constraintStr = map (\(LEQConstraint (Expr v1 n1) (Expr v2 n2)) -> v1 ++ "," ++ show n1 ++ "<" ++ v2 ++ "," ++ show n2) constraints
        fullFile = intercalate "\n" $ concat [[constraintsName], constraintStr, ["#SOLUTION#"]]
        humanReadable = intercalate "\n" $ map (\(LEQConstraint (Expr v1 n1) (Expr v2 n2)) -> v1 ++ " " ++ showInt n1 ++ " <= " ++ v2 ++ " " ++ showInt n2) constraints
    writeFile ("constraints/" ++ fileName ++ ".time") fullFile
    writeFile ("constraints/" ++ fileName ++ ".htime") humanReadable
    where showInt i = if i >= 0 
                      then "+ " ++ show i
                      else "- " ++ show (0 - i)
          

-- write a time constraint set to file in the correct format with .time, human-readable with .htime
writeTimeConstraintsToFile2 :: String -> String -> [Constraint] -> IO ()
writeTimeConstraintsToFile2 filePath constraintsName constraints = do
    let constraintStr = map (\(LEQConstraint (Expr v1 n1) (Expr v2 n2)) -> v1 ++ "," ++ show n1 ++ "<" ++ v2 ++ "," ++ show n2) constraints
        fullFile = intercalate "\n" $ concat [[constraintsName], constraintStr, ["#SOLUTION#"]]
        humanReadable = intercalate "\n" $ map (\(LEQConstraint (Expr v1 n1) (Expr v2 n2)) -> v1 ++ " " ++ showInt n1 ++ " <= " ++ v2 ++ " " ++ showInt n2) constraints
    writeFile (filePath ++ ".tc") fullFile
    writeFile (filePath ++ ".hc") humanReadable
    where showInt i = if i >= 0 
                      then "+ " ++ show i
                      else "- " ++ show (0 - i)
          
{-

    A few simple examples, from the writeup. see what it does. if anything.
-}

                              
fileSetup fileNamePrefix constraints = do
    fileG ("graphs/" ++ fileNamePrefix ++ "-G.dg") constraints
    fileH ("graphs/" ++ fileNamePrefix ++ "-H-1.dg") constraints
    fileHAssigned ("graphs/" ++ fileNamePrefix ++ "-H-2.dg") constraints
    
    where fileG fileName constraints = exprGraphGToFile fileName $ buildGraphG constraints
    
          fileH fileName constraints = case constraintsToGraphH constraints of
             Left _ -> putStrLn "Could not create graph h"
             Right h -> componentGraphHToFile fileName h
             
          fileHAssigned fileName constraints = case solveConstraintsAndReturnGraphAndWeights constraints of
             Left _ -> putStrLn "Could not write results graph H"
             Right (graphHData, vertexWeights, edgeWeights) -> solvedGraphHToFile fileName graphHData vertexWeights edgeWeights


fileSetup2 filePath constraints = do
    fileG (filePath ++ "-G.dg") constraints
    fileH (filePath ++ "-H-1.dg") constraints
    fileHAssigned (filePath ++ "-H-2.dg") constraints
    
    where fileG fileName constraints = exprGraphGToFile fileName $ buildGraphG constraints
    
          fileH fileName constraints = case constraintsToGraphH constraints of
             Left _ -> return () -- putStrLn "Could not create graph h"
             Right h -> componentGraphHToFile fileName h
             
          fileHAssigned fileName constraints = case solveConstraintsAndReturnGraphAndWeights constraints of
             Left _ -> return () -- putStrLn "Could not write results graph H"
             Right (graphHData, vertexWeights, edgeWeights) -> solvedGraphHToFile fileName graphHData vertexWeights edgeWeights

             
{-
    example141C,...,example147C
    example151C,...,example158C
    from the pdf
-}


fileAllExamples = do
    file141
    file142
    file143
    file144
    file145
    file146
    file147
    file151
    file152
    file153
    file154
    file155
    file156
    file157
    file158


file141 = fileSetup "141" example141C
file142 = fileSetup "142" example142C
file143 = fileSetup "143" example143C
file144 = fileSetup "144" example144C
file145 = fileSetup "145" example145C
file146 = fileSetup "146" example146C
file147 = fileSetup "147" example147C

file151 = fileSetup "151" example151C
file152 = fileSetup "152" example152C
file153 = fileSetup "153" example153C
file154 = fileSetup "154" example154C
file155 = fileSetup "155" example155C
file156 = fileSetup "156" example156C
file157 = fileSetup "157" example157C
file158 = fileSetup "158" example158C

fileWorstCaseAlg1 = fileSetup "WorstCaseAlg1" worstCaseAlg1
          
run141 = runPC ("Example 1.4.1", example141C)
run142 = runPC ("Example 1.4.2", example142C)
run143 = runPC ("Example 1.4.3", example143C)
run144 = runPC ("Example 1.4.4", example144C)
run145 = runPC ("Example 1.4.5", example145C)
run146 = runPC ("Example 1.4.6", example146C)
run147 = runPC ("Example 1.4.7", example147C)

run151 = runPC ("Example 1.5.1", example151C)
run152 = runPC ("Example 1.5.2", example152C)
run153 = runPC ("Example 1.5.3", example153C)
run154 = runPC ("Example 1.5.4", example154C)
run155 = runPC ("Example 1.5.5", example155C)
run156 = runPC ("Example 1.5.6", example156C)
run157 = runPC ("Example 1.5.7", example157C)
run158 = runPC ("Example 1.5.8", example158C)

test14s = testBatchC [("Example 1.4.1", example141C, example141S),
                      ("Example 1.4.2", example142C, example142S),
                      ("Example 1.4.3", example143C, example143S),
                      ("Example 1.4.4", example144C, example144S),
                      ("Example 1.4.5", example145C, example145S),
                      ("Example 1.4.6", example146C, example146S),
                      ("Example 1.4.7", example147C, example147S)]


test15s = testBatchC [("Example 1.5.1", example151C, example151S),
                      ("Example 1.5.2", example152C, example152S),
                      ("Example 1.5.3", example153C, example153S),
                      ("Example 1.5.4", example154C, example154S),
                      ("Example 1.5.5", example155C, example155S),
                      ("Example 1.5.6", example156C, example156S),
                      ("Example 1.5.7", example157C, example157S),
                      ("Example 1.5.8", example158C, example158S)]



test141 = testPC ("Example 1.4.1", example141C, example141S)
test142 = testPC ("Example 1.4.2", example142C, example142S)
test143 = testPC ("Example 1.4.3", example143C, example143S)
test144 = testPC ("Example 1.4.4", example144C, example144S)
test145 = testPC ("Example 1.4.5", example145C, example145S)
test146 = testPC ("Example 1.4.6", example146C, example146S)
test147 = testPC ("Example 1.4.7", example147C, example147S)

test151 = testPC ("Example 1.5.1", example151C, example151S)
test152 = testPC ("Example 1.5.2", example152C, example152S)
test153 = testPC ("Example 1.5.3", example153C, example153S)
test154 = testPC ("Example 1.5.4", example154C, example154S)
test155 = testPC ("Example 1.5.5", example155C, example155S)
test156 = testPC ("Example 1.5.6", example156C, example156S)
test157 = testPC ("Example 1.5.7", example157C, example157S)
test158 = testPC ("Example 1.5.8", example158C, example158S)


example141C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "B" 1),
                                       LEQConstraintI (Expr "A" 0) (Expr "B" 0)]
                                      
example142C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "B" (-1)),
                                       LEQConstraintI (Expr "A" 0) (Expr "B" 0)]
                                      
example143C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "B" (-2)),
                                       LEQConstraintI (Expr "A" 0) (Expr "B" 0)]
                                      
                                      
example144C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "D" 0),
                                       LEQConstraintI (Expr "A" 0) (Expr "B" 0),
                                       EQConstraintI (Expr "C" 0) (Expr "B" (-1)),
                                       EQConstraintI (Expr "D" 0) (Expr "C" 1)]
                                       
                                       
example145C = convertInputConstraints [EQConstraintI (Expr "E" 0) (Expr "B" (-1)),
                                       EQConstraintI (Expr "C" 0) (Expr "E" 0),
                                       EQConstraintI (Expr "C" 0) (Expr "D" 0),
                                       EQConstraintI (Expr "A" 0) (Expr "C" 0)]
                                       
                                       
example146C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "F" 0), 
                                       EQConstraintI (Expr "F" 0) (Expr "B" 0), 
                                       EQConstraintI (Expr "B" 0) (Expr "C" 0), 
                                       EQConstraintI (Expr "D" 0) (Expr "G" 1), 
                                       EQConstraintI (Expr "E" 0) (Expr "H" 1), 
                                       EQConstraintI (Expr "I" 0) (Expr "J" 0), 
                                       EQConstraintI (Expr "K" 0) (Expr "L" 0), 
                                       EQConstraintI (Expr "N" 0) (Expr "K" (-1)), 
                                       EQConstraintI (Expr "M" 0) (Expr "I" 0), 
                                       EQConstraintI (Expr "M" 0) (Expr "N" 0), 
                                       EQConstraintI (Expr "C" 0) (Expr "M" 0), 
                                       LEQConstraintI (Expr "B" 0) (Expr "G" 0), 
                                       LEQConstraintI (Expr "C" 0) (Expr "H" 0), 
                                       LEQConstraintI (Expr "A" 0) (Expr "I" 0), 
                                       LEQConstraintI (Expr "C" 0) (Expr "J" 0), 
                                       LEQConstraintI (Expr "D" 0) (Expr "K" 0), 
                                       LEQConstraintI (Expr "E" 0) (Expr "L" 0)]
                                       
                                       
                                       
example147C = convertInputConstraints [EQConstraintI (Expr "D" 0) (Expr "A" 0),
                                       EQConstraintI (Expr "A" 0) (Expr "B" 0),
                                       EQConstraintI (Expr "B" 0) (Expr "C" 0),
                                       EQConstraintI (Expr "G" 0) (Expr "F" 0),
                                       EQConstraintI (Expr "L" 0) (Expr "H" 1),
                                       EQConstraintI (Expr "J" 0) (Expr "K" 0),
                                       EQConstraintI (Expr "M" 0) (Expr "J" 1),
                                       EQConstraintI (Expr "L" 0) (Expr "I" 0),
                                       EQConstraintI (Expr "L" 0) (Expr "M" 0),
                                       EQConstraintI (Expr "N" 0) (Expr "L" (-1)),
                                       EQConstraintI (Expr "E" 0) (Expr "G" 0),
                                       EQConstraintI (Expr "E" 0) (Expr "N" 0),
                                       EQConstraintI (Expr "C" 0) (Expr "E" 0),
                                       LEQConstraintI (Expr "B" 0) (Expr "F" 0),
                                       LEQConstraintI (Expr "A" 0) (Expr "H" 0),
                                       LEQConstraintI (Expr "C" 0) (Expr "I" 0),
                                       LEQConstraintI (Expr "B" 0) (Expr "K" 0)]
                                       
                                       
example151C = convertInputConstraints [EQConstraintI (Expr "K" 1) (Expr "J" 0),
                                       EQConstraintI (Expr "K" 3) (Expr "J" 1)]
                                       
                                       
example152C = convertInputConstraints [EQConstraintI (Expr "K" 1) (Expr "J" 0),
                                       EQConstraintI (Expr "K" 2) (Expr "J" 1),
                                       EQConstraintI (Expr "K" 3) (Expr "J" 2)]
                                       
example153C = convertInputConstraints [EQConstraintI (Expr "K" 0) (Expr "M" 0),
                                       EQConstraintI (Expr "K" 2) (Expr "J" 1),
                                       EQConstraintI (Expr "M" 2) (Expr "J" 0)]
                                       
example154C = convertInputConstraints [LEQConstraintI (Expr "J" 0) (Expr "K" 0),                                       LEQConstraintI (Expr "K" 1) (Expr "J" 0)]

example155C = convertInputConstraints [LEQConstraintI (Expr "J" 0) (Expr "K" 0),                                       LEQConstraintI (Expr "J" 1) (Expr "K" 0)]

example156C = convertInputConstraints [EQConstraintI (Expr "A" 0) (Expr "B" (-1)),
                                       EQConstraintI (Expr "B" 0) (Expr "C" (-1)),
                                       EQConstraintI (Expr "C" 0) (Expr "D" (-1))]
                                       
example157C = convertInputConstraints [LEQConstraintI (Expr "A" 0) (Expr "B" (-1)),
                                       LEQConstraintI (Expr "B" 0) (Expr "C" (-1)),
                                       LEQConstraintI (Expr "C" 0) (Expr "D" (-1))]
                                       
example158C = convertInputConstraints [LEQConstraintI (Expr "B" (-1)) (Expr "A" 0),
                                       LEQConstraintI (Expr "C" (-1)) (Expr "B" 0),
                                       LEQConstraintI (Expr "D" (-1)) (Expr "C" 0)]
                     
                     
worstCaseAlg1 = convertInputConstraints [LEQConstraintI (Expr "X1" 0) (Expr "X2" 0),
                                                LEQConstraintI (Expr "X2" 0) (Expr "X3" 0),
                                                LEQConstraintI (Expr "X3" 0) (Expr "X4" 0),
                                                LEQConstraintI (Expr "X2" 1) (Expr "X3" 1),
                                                LEQConstraintI (Expr "X3" 1) (Expr "X4" 1),
                                                LEQConstraintI (Expr "X1" 0) (Expr "X0" 0),
                                                LEQConstraintI (Expr "X0" 2) (Expr "X1" 1)]
                     
-- Solutions                     
example141S = Nothing
                                       
example142S = Just $ M.fromList [("A", 0),
                                 ("B", 1)]
                    
example143S = Just $ M.fromList [("A", 0),
                                 ("B", 2)]
                    
example144S = Just $ M.fromList [("A", 1),
                                 ("B", 1),
                                 ("C", 0),
                                 ("D", 1)]
                    
example145S = Just $ M.fromList [("A", 0),
                                 ("B", 1),
                                 ("C", 0),
                                 ("D", 0),
                                 ("E", 0)]
                    
                    
example146S = Just $ M.fromList [("A", 0),
                                 ("B", 0),
                                 ("C", 0),
                                 ("D", 1),
                                 ("E", 1),
                                 ("F", 0),
                                 ("G", 0),
                                 ("H", 0),
                                 ("I", 0),
                                 ("J", 0),
                                 ("K", 1),
                                 ("L", 1),
                                 ("M", 0),
                                 ("N", 0)]
                    
example147S = Just $ M.fromList [("A", 0),
                                 ("B", 0),
                                 ("C", 0),
                                 ("D", 0),
                                 ("E", 0),
                                 ("F", 0),
                                 ("G", 0),
                                 ("H", 0),
                                 ("I", 1),
                                 ("J", 0),
                                 ("K", 0),
                                 ("L", 1),
                                 ("M", 1),
                                 ("N", 0)]
                    
example151S = Nothing

example152S = Just $ M.fromList [("J", 1),
                                 ("K", 0)]
                    
example153S = Nothing

example154S = Nothing

example155S = Just $ M.fromList [("J", 0),
                                 ("K", 1)]
                    
example156S = Just $ M.fromList [("A", 0),
                                 ("B", 1),
                                 ("C", 2),
                                 ("D", 3)]
                    
example157S = Just $ M.fromList [("A", 0),
                                 ("B", 1),
                                 ("C", 2),
                                 ("D", 3)]
                    
example158S = Just $ M.fromList [("A", 0),
                                 ("B", 1),
                                 ("C", 1),
                                 ("D", 1)]