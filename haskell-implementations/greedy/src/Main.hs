module Main where

{-

Explicitly-typed lambda calculus with parametric polymorphism and let-bindings.

-}

import TypeInference
import Tokens
import ParsingInterface
import Errors
import System.Directory
import System.Environment

import Data.Map.Strict (empty, Map, insert, (!?))
import Data.List (elemIndex, foldl', elemIndices)
import Data.Either
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict



import Control.Monad.Trans.Except
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad

import Debug.Trace

import System.IO (stdin, stdout, BufferMode(..), hSetBuffering)

import qualified Data.Set as S

          
-- For riplG       
{-   
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    ripl
-}
-- for friplG
{-
main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    args <- getArgs
    case args of
        [] -> putStrLn "Please give the name of the RIPL output folder e.g. test, kbfuncs"
        [x] -> writerRIPL x
        (x:_) -> putStrLn "Please give the name of only one RIPL output folder"
-}

-- Basic check

        
testType :: IO ()
testType = getLine >>= printType . buildAndCheckType

testTypes :: IO ()
testTypes = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    l <- getLine
    if l == "Q" || l == "q"
    then return ()
    else do
        printType . buildAndCheckType $ l
        putStrLn ""
        testTypes
    

printType :: Either Error Type -> IO ()
printType = let l err = show err
                r t = "Success: " ++ show t
            in putStrLn . either l r
          
            
scriptTypes :: FilePath -> IO ()
scriptTypes file = do
    exprs <- fmap (map buildExpr . filter (/= "") . lines) (readFile file)
    mapM_ f exprs
  where f expr = do
          print expr
          printType $ runInfer2 expr
          putStrLn ""
          
          
-- A test file for us is defined as a list of lines, format alternating
--   lc-expr-string
--   expected-type-string, preceded by either "S: " or "F: " depending on whether the test should succeed or fail
-- where all intermediate empty lines are removed before pairing expressions with their expected types
-- we also include basic comments - filter out all lines that start "--"


testFile :: FilePath -> IO ()
testFile file = do
    putStrLn ""
    testLines <- fmap (filter notEmptyOrComment . lines) (readFile file)
    either print succ (testResults testLines)
    where succ eitherList = do
                displayTests eitherList
                let (testCount, failures) = (length eitherList, length $ lefts eitherList)
                putStr $ show testCount ++ " tests complete"
                if failures == 0
                then putStrLn "d successfully.\n"
                else putStrLn $ " with " ++ show failures ++ " failures.\n"
          -- testResults :: [String] -> Either String [Either String ()]
          testResults testLines = do
               testLines' <- macroSubstitutions testLines
               runTests <$> testType testLines'
          notEmptyOrComment = ((&&) <$> (/= "") <*> ((/= "--") . take 2))
          testType :: [String] -> Either Error [(String,String)]
          testType fileLines = 
              case fileLines of 
                  "#SINGLE" : ls -> pairAdjacent ls
                  "#MULTI" : ls -> constructMultilineTests ls
                  _     -> Left $ TestParseFailure "Test file didn't start with #SINGLE or #MULTI"
          
pairAdjacent :: [a] -> Either Error [(a,a)]
pairAdjacent [] = Right []
pairAdjacent [x1] = Left $ TestParseFailure "Expected paired elements in a list; got an odd number of elements (maybe a test without an expected type?)"
pairAdjacent (x1 : x2 : xs) = do
    ls <- pairAdjacent xs
    return $ (x1, x2) : ls
          
          
          
-- A multiline test is specified as follows:
-- #TEST
-- <lcexpression on multiple lines>
-- #EXPECTED
-- <single line type>
-- #END
-- where the multi-line lc-expression is reduced to a single line by
-- removing each newline \n and all its following whitespace.
-- A bit crude, to be sure. Anything more complicated probably needs
-- a parser of its own.

constructMultilineTests :: [String] -> Either Error [(String,String)]
constructMultilineTests ss = fmap (map (\(v1,v2) -> (whitespaceStripConcat v1, id v2))) $ produceTestPair ss

   
produceTestPair :: [String] -> Either Error [([String], String)]
produceTestPair [] = Right []
produceTestPair ("#TEST" : ls) = do
    (toMerge, rem) <- readToMarker "#EXPECTED" ls
    (expected, rem') <- readToMarker "#END" rem
    others <- produceTestPair rem'
    if length expected == 1
    then return $ (toMerge, head expected) : others
    else Left $ TestParseFailure $ "Test parsing error: in #EXPECTED, got more than one line: " ++ show expected
produceTestPair _ = Left $ TestParseFailure $ "Test parsing error: expected a test #TEST"


readToMarker :: String -> [String] -> Either Error ([String],[String])
readToMarker marker [] = Left $ TestParseFailure $ "Test parsing error: expected the marker " ++ marker
readToMarker marker (x:xs) = 
    if marker == x
    then Right ([], xs)
    else let rem = readToMarker marker xs
         in fmap (\(l1, l2) -> (x:l1, l2)) rem


         
whitespaceStripConcat :: [String] -> String
whitespaceStripConcat = concat . map (dropWhile (\c -> c == ' ' || c == '\n' || c == '\t'))
          
          
          
          
data ExpectedTestResult = TestPass Type | TestFailsToTypeCheck


runTests :: [(String, String)] -> [Either String ()]
runTests = map f
    where f (lcExprStr, testTypeStr) = 
              let resultTypeE = buildAndCheckType $ lcExprStr
                  expectedE = parseExpected testTypeStr
              in case (resultTypeE, expectedE) of
                    (Right resultType, Right (TestPass expectedType)) -> 
                        if typeEqualsRename S.empty resultType expectedType
                        then Right ()
                        else Left $ "FAILURE: in expression " ++ lcExprStr ++ ", got different expected and result types:\nExpected: " ++ show expectedType ++ "\nResult: " ++ show resultType
                        
                    (Right resultType, Right TestFailsToTypeCheck) -> Left $ "FAILURE: in expression " ++ lcExprStr ++ ", expected a type-check failure but got type " ++ show resultType
                    
                    (Left err1, Right (TestPass expectedType)) -> Left $ "FAILURE: in expression " ++ lcExprStr ++ ", expected success with type " ++ show expectedType ++ " but failed with error:\n\t" ++ show err1
                    
                    (Left (TypeCheckFailure _), Right TestFailsToTypeCheck) -> Right ()
                    
                    (Left err1, Right TestFailsToTypeCheck) -> Left $ "FAILURE: in expression " ++ lcExprStr ++ ", expected type checking failure but got some other failure:\n\t" ++ show err1
                    
                    (Right resultType, Left err2) -> Left $ "FAILURE: in expression " ++ lcExprStr ++ ", although the type check was successful, the expected type failed to parse with error:\n\t" ++ show err2
                    
                    (Left err1, Left err2) -> Left $ "FAILURE: in expression " ++ lcExprStr ++ ". The type check failed with error message\n\t" ++ show err1 ++ "\nand the expected type failed to parse with error:\n\t" ++ show err2
          
          parseExpected str = 
              -- For now, just have "S: " indicating success for given type and anything else indicating failure
              if (take 3 str == "S: ")
              then fmap TestPass $ buildType $ drop 3 str
              else Right TestFailsToTypeCheck



displayTests :: [Either String ()] -> IO ()
displayTests l = mapM_ f l 
    where f (Right _) = return ()
          f (Left err) = putStrLn err >> putStrLn ""
          
scriptTemp = scriptTypes "src/test.lc"
          
tests = testFile "src/fullTest.lct"
          
tests2 = testFile "src/fullTest2.lct"          
          
          


  
  
-- Takes a list of (non-empty) strings
-- In order, goes through the strings looking for macro strings starting with %<name>=<lcexpr>
-- Whenever it sees a macro string, it binds <name> to <lcexpr> in a map
-- then in the remainder of the string list, replaces all occurrences of "$<name>$" with "(<lcexpr>)"
-- Note that recursive substitutions are not possible.
macroSubstitutions :: [String] -> Either Error [String]
macroSubstitutions = fmap (reverse . snd) . foldM f (empty, [])
    where f :: (Map String String, [String]) -> String -> Either Error (Map String String, [String])
          f (macros, currentStrs) str = do
                str' <- macroSubstitution str macros
                (macros', wasMacro) <- updateMacros str' macros
                return (macros', if wasMacro then currentStrs else str' : currentStrs)

          
-- macro substitution operates as follows:
-- store some 'prefix' up to the first '$' of a pair (including up to end)
-- store the characters up to the next '$' as a name (fail if no next '$')
-- look up the name in the map to get the replacement string s
-- concat the prefix with s then with the macroSubstitution of everything after the next '$'
macroSubstitution :: String -> Map String String -> Either Error String
macroSubstitution str macros = do
    case '$' `elemIndex` str of
        Just i -> do
            let prefix = take i str
                suffix = drop (i+1) str
            (name, afterName) <- extractName suffix
            let nameNotFound = "Macro parsing failure: could not find macro substitution for name " ++ name ++ " in string " ++ str
            toInsert <- toEither (MacroFailure nameNotFound) $ macros !? name
            str' <- macroSubstitution afterName macros
            return $ prefix ++ "(" ++ toInsert ++ ")" ++ str'
            
        Nothing -> return str

                                   
    
toEither :: a -> Maybe b -> Either a b
toEither a Nothing = Left a
toEither _ (Just b) = Right b
    
extractName :: String -> Either Error (String, String)
extractName str' = 
    case '$' `elemIndex` str' of
        Just i -> Right $ (take i str', drop (i+1) str')
        Nothing -> Left $ MacroFailure $ "Macro parsing failure:  could not find closing '$' in string " ++ str'
    

-- Returns the new macro map, plus whether or not this was a macro (so it can be ignored)
updateMacros :: String -> Map String String -> Either Error (Map String String, Bool)
updateMacros str macros = 
    if take 1 str == "%"
    then case '=' `elemIndex` str of
            Just 1 -> Left $ MacroFailure $ "Macro parsing failure: it helps if, when defining a macro, you give that macro a name."
            Just i -> let name = drop 1 . take i $ str
                          val = drop (i + 1) str
                      in Right $ (insert name val macros, True)
            Nothing -> Left $ MacroFailure $ "Macro parsing failure: failed to find '=' after macro start in " ++ str
    else Right (macros, False)



    
{-

    REPL COPIED WHOLESALE FROM THE B&K VERSION, MODIFIED TO NOT HAVE TIMES!
-}
whitespaceTrimAndMergeNoNewLine :: String -> String
whitespaceTrimAndMergeNoNewLine s =
    let replaceTab = map (\c -> if c == '\t' then ' ' else c) s
        merged = merge replaceTab
        -- trimmed = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ') $ replaceTab
    in dropWhile (== ' ') merged
    where merge "" = ""
          merge (' ':s) = 
            let rem = merge $ dropWhile (== ' ') s
            in ' ':rem
          merge (c:s) = c:merge s
    

-- REPLs - allow binding of variables and multiline inputs
-- multiline inputs are stripped down to
-- standard REPL, doesn't save any test results to file
type Gamma = M.Map String Type
type InputString = String
type NextTimeVarNum = Integer
type IterationCount = Integer
type RIPLInput = (InputString, Gamma, IterationCount)

runRIPL :: Gamma -> (RIPLInput -> IO (Maybe Type)) -> Integer -> IO ()
runRIPL gammaInit riplAction n = do
    putStrLn ""
    l <- getLine
    if l == "Q" || l == "q"
    then return ()
    else do
       input <- multiline l
       let gammaInit' = freshGammaInit gammaInit
           n' = n + 1
       case binding input of
            Nothing -> putStrLn "Binding must be in format @name=body" >> runRIPL gammaInit' riplAction n'
            
            Just (Left input) -> do
                _ <- riplAction (input, gammaInit', n)
                runRIPL gammaInit' riplAction n'
                
            Just (Right (name, bodyInput)) -> do
                mt <- riplAction (bodyInput, gammaInit', n)
                case mt of
                    Nothing -> runRIPL gammaInit' riplAction n
                    Just t -> runRIPL (bindTypeInGamma name t gammaInit') riplAction n'
    
    
-- standard repl - don't save files
ripl :: IO ()
ripl = do
    putStrLn "Simple `greedy' inference: standard RIPL, no file output; q or Q to quit"
    runRIPL builtInAbsType basicRIPL 1
    
    
-- folder structure:
-- replout/filePrefix/  is main folder
--   for item entered in iteration i,  filePrefix/i-type.txt is the expression and the resulting type, if any, or an error
--                                     filePrefix/graphviz/i-G.dg           respective graphs, GraphViz dot-graph format
--                                     filePrefix/graphviz/i-H-1.dg  
--                                     filePrefix/graphviz/i-H-2.dg  
--                                     filePrefix/timeconstraints/i-times.tc        time constraints, for haskell reading
--                                     filePrefix/timeconstraints/i-times.hc       time constraints, for human reading
--                                     filePrefix/typeconstraints/i-types.ty        type constraints
writerRIPL :: String -> IO ()
writerRIPL filePrefix = do
    putStrLn $ "Simple `greedy' inference: file-writer RIPL, result output to directory /" ++ filePrefix ++ ", ; q or Q to quit"
    ensureDirectories
    runRIPL builtInAbsType (writerRIPLAction ("results/" ++ filePrefix)) 1
    
    where ensureDirectories = do
            createDirectoryIfMissing True ("results/" ++ filePrefix ++ "/")
                
-- type GammaWithTimes = (M.Map String GammaElem, M.Map String Integer)  
--                                                  name -> inferred time
-- type RIPLInput = (InputString, GammaWithTimes)
-- inferTypeFromGammaAndTimeConstraints :: Expr -> M.Map String GammaElem -> TimeConstraints -> Either Error (Type, TimeStep)
basicRIPL :: RIPLInput -> IO (Maybe Type)
basicRIPL (input, gammaInit, iter) = do
    case buildExprUsingExplicitBuiltInNames (S.fromList . M.keys $ gammaInit) input of
        Left err -> (putStrLn . show $ err) >> return Nothing
        Right expr -> do
            let inferred = inferTypeFromGamma expr gammaInit
            putStr $ "(" ++ show iter ++ ") "
            printType inferred
            case inferred of
                Left _ -> return Nothing
                Right typ -> return $ Just typ
    
    


writerRIPLAction :: String -> RIPLInput -> IO (Maybe Type)
writerRIPLAction filePrefix (input, gammaInit, currentIteration) = do
    case buildExprUsingExplicitBuiltInNames (S.fromList . M.keys $ gammaInit) input of
        Left err -> (putStrLn . show $ err) >> return Nothing
        Right expr -> do
            let inferred = inferTypeFromGamma expr gammaInit
            putStr $ "(" ++ show currentIteration ++ ") "
            printType inferred
            case inferred of
                Left err -> do
                    writeTypeToFile (filePrefix ++ "/" ++ show currentIteration ++ "-type.txt") input (show err)
                    return Nothing
                Right typ -> do
                    writeTypeToFile (filePrefix ++ "/" ++ show currentIteration ++ "-type.txt") input (show typ)
                    return $ Just typ
    
    
writeTypeToFile :: String -> String -> String -> IO ()
writeTypeToFile filePath typeString resultString = do
    writeFile filePath ("Input:\n" ++ typeString ++ "\n\nResult:\n" ++ resultString)

                    
                
-- For every type in gamma, renames according to a fresh set of type and time variables
-- maybe not actually necessary due to type scheme automatically refreshing it!
-- but still useful when code will be copied into the other two implementations
-- each is given a fresh time variable directly, no renaming needed
--                                                       vv next time variable available
freshGammaInit :: Gamma -> Gamma
freshGammaInit gammaInit = freshenTypeVars gammaInit
    where               
        -- all elements of gamma are of no variables bound, they're all empty type schemes
          freshenTypeVars  :: M.Map String Type -> M.Map String Type
          freshenTypeVars g = fst $ M.foldrWithKey foldFreshen (M.empty, 0) g

          foldFreshen :: String -> Type -> (Gamma, Integer) -> (Gamma, Integer)
          foldFreshen name t (m, n) = 
              let (freshened, nextTypeVar) = doFreshen t n
              in (M.insert name freshened m, nextTypeVar + 1)

          doFreshen :: Type -> Integer -> (Type, Integer)
          doFreshen t nextTypeVar = 
              let (t', (_, nextTypeVar')) = runState (doFreshen' t) (M.empty, nextTypeVar)
              in (t', nextTypeVar')

          doFreshen' :: Type -> State (M.Map String String, Integer) Type
          doFreshen' tc@(TypeConstant _) = return tc
          doFreshen' (TypeConstructor s ts) = do
              ts' <- mapM doFreshen' ts
              return $ TypeConstructor s ts' 
          doFreshen' (FunctionType fr to) = do
              fr' <- doFreshen' fr
              to' <- doFreshen' to
              return $ FunctionType fr to
        
          doFreshen' (TupleType ts) = do
              ts' <- mapM doFreshen' ts
              return $ TupleType ts' 
        
          doFreshen' (TypeVariable tv) = do
              (m, next) <- get
              case m M.!? tv of
                  Nothing -> do
                      let newTypeVar = "?T" ++ show next
                          next' = next + 1
                          m' = M.insert tv newTypeVar m
                      put (m', next')
                      return $ TypeVariable newTypeVar
                  Just replacement -> return $ TypeVariable replacement
       
{-
data Type = TypeConstant String
          | TypeVariable String
          | TypeConstructor String [Type]
          | FunctionType { fFrom :: Type, fTo :: Type }
          | TupleType [Type] 
          | UntypedVariable 
          | BulletType Type deriving (Eq, Ord)
-}




                
    
    
bindTypeInGamma :: String -> Type -> M.Map String Type -> M.Map String Type
bindTypeInGamma name t = M.insert name t -- doesn't matter what type or time names are here, we'll refresh them every iteration
    
    
-- single-line input to Maybe (Either <body> (<name>, <body>)) where Left is no binding, Right is binding
-- binding is @<name>=<binding>, <name> is alphabetical
-- result is Nothing if a binding was started but never finished
binding :: String -> Maybe (Either String (String, String))
binding [] = Just $ Left []
binding ('@':s) = 
    let (name,rem) = span (`elem` (['a'..'z'] ++ ['A'..'Z'])) s
    in case rem of
            ('=':body) -> Just $ Right (name, body)
            _ -> Nothing
binding (c:s) = Just $ Left $ c:s

    
-- multi-line input coordinator, also trims and merges spaces
-- i *think* it'll never read a \n character from getLine
-- start and end multiline with character '#'; discards everything on the line after the closing '#'
multiline :: String -> IO String
multiline ('#':s) = do
      rest <- readUntilHash
      return . whitespaceTrimAndMergeNoNewLine $ s ++ rest
    where readUntilHash = do
            l <- fmap (dropWhile (\c -> c == ' ' || c == '\t')) getLine     -- strip leading whitespace as we go
            if '#' `elem` l
            then return $ takeWhile (/= '#') l
            else do
              l' <- readUntilHash
              return $ l ++ l'

multiline s = return . whitespaceTrimAndMergeNoNewLine $ s -- any non-# start just treat as a single line


    
    
    
-- Test types
tT = FunctionType (TypeVariable "T") (TypeVariable "T")

tT1 = FunctionType (TypeConstant "Bool") (TypeConstant "Bool")

tT2 = FunctionType (TypeVariable "U") (TypeVariable "U")

tT3 = FunctionType (TypeVariable "U1") (TypeVariable "U2")

tT4 = FunctionType (TypeConstant "Bool") (TypeVariable "U")

tT5 = FunctionType (TypeVariable "U") (TypeConstant "Bool")

t1 = match' S.empty tT tT1
t2 = match' S.empty tT tT2
t3 = match' S.empty tT tT3
t4 = match' S.empty tT tT4
t5 = match' S.empty tT tT5