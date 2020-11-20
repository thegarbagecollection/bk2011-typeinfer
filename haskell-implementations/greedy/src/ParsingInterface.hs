{-
    Hiding all the parsing details away from the tests in Main
-}

module ParsingInterface where
import Errors
import Tokens
import Parser
import TypeParser
import Lexer
import Renaming
import TypeInference
import Data.Map.Strict (empty, Map, insert, (!?))
import Control.Monad.Trans.State.Strict
import Control.Monad ((>=>))
import qualified Data.Set as S


parseAndInfer :: String -> Either Error Type
parseAndInfer = alexScanTokens >=> parseRawExpr >=> toExpr >=> runInfer

buildAndCheckType = buildExpr >=> runInfer

parseRaw = do
    line <- getLine 
    case alexScanTokens line of
        Right tok -> case parseRawExpr tok of
                        Right rexp -> print rexp
                        Left err -> print err
        Left err -> print err

parseExp = do
    line <- getLine 
    case alexScanTokens line of 
        Right tok -> case parseRawExpr tok of
                        Right rexp -> print . toExpr $ rexp
                        Left err -> print err
        Left err -> print err
        
        
buildExpr :: String -> Either Error Expr
buildExpr s = do
    tok <- alexScanTokens s
    rExpr <- parseRawExpr tok
    toExprInit (S.fromList builtInAbs) rExpr 
        

buildExprUsingExplicitBuiltInNames :: S.Set String -> String -> Either Error Expr
buildExprUsingExplicitBuiltInNames initialAbsNames s = do
    tok <- alexScanTokens s
    rExpr <- parseRawExpr tok
    toExprInit initialAbsNames rExpr
        
          
buildType :: String -> Either Error Type
buildType s = do
    tok <- alexScanTokens s
    typeLookup <- parseType tok
    fmap fst $ evalStateT (getType "<TYPE STRING>" . Just $ typeLookup) (VC (empty, empty) empty)
          