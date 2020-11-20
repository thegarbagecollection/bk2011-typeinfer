module ParsingInterface where
import Tokens
import TypeParser
import Lexer
import Parser
import Renaming
import Errors
import Data.Map.Strict (empty, Map, insert, (!?))
import Data.List (elemIndex, foldl', elemIndices)
import qualified Data.Set as S
import Control.Monad.Trans.State.Strict
import TypeTimeInference

inferFromExpr = runInference . buildExpr


-- Basic check
parseRaw = do
    line <- getLine 
    case alexScanTokens line of
        Right tok -> case parseLCT tok of
                        Right rexp -> print rexp
                        Left err -> print err
        Left err -> print err

parseExp = do
    line <- getLine 
    case alexScanTokens line of 
        Right tok -> case parseLCT tok of
                        Right rexp -> print . toExpr (S.fromList builtInAbs) $ rexp
                        Left err -> print err
        Left err -> print err
        
        
buildExpr :: String -> Either Error Expr
buildExpr s = do
    tok <- alexScanTokens s
    rExpr <- parseLCT tok
    toExpr (S.fromList builtInAbs) rExpr 
        

buildExprUsingExplicitBuiltInNames :: S.Set String -> String -> Either Error Expr
buildExprUsingExplicitBuiltInNames initialAbsNames s = do
    tok <- alexScanTokens s
    rExpr <- parseLCT tok
    toExpr initialAbsNames rExpr 

          
buildType :: String -> Either Error Type
buildType s = do
    tok <- alexScanTokens s
    typeLookup <- parseType tok
    fmap fst $ evalStateT (getType "<TYPE STRING>" . Just $ typeLookup) (VC (empty, empty) empty)
    
    
    
rawType :: String -> Either Error TypeLookup
rawType s = do
    tok <- alexScanTokens s
    parseType tok