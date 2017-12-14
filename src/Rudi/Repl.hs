module Rudi.Repl
    (
        repl,
        replFile
    ) where

import Data.Map (Map)
import qualified Data.Map as Map

import Text.ParserCombinators.Parsec

import Rudi.Interpreter
import Rudi.Parser
import Rudi.Types

evaluateFile :: RudiFile -> IO (Map Expr Expr)
evaluateFile (RudiFile []) = return Map.empty
evaluateFile (RudiFile (EmptyStatement:statements)) = evaluateFile (RudiFile statements)
evaluateFile (RudiFile (Import path:statements)) = do
    m <- evaluateFile $ RudiFile statements

    loadedMap <- loadFile (path ++ ".rudi")

    return $ Map.union m loadedMap

evaluateFile (RudiFile (Define x y:statements)) = do
    m <- evaluateFile $ RudiFile statements

    return $ Map.insert x y m

loadFile :: String -> IO (Map Expr Expr)
loadFile path = evaluateFile =<< (parseFile <$> readFile path)

runRepl :: Map Expr Expr -> IO ()
runRepl defs = do
    putStr "> "
    line <- getLine

    if unwords (words line) == "" then
        runRepl defs
    else
        case parse rudiStatement "" line of
            Right statement -> do
                newDefs <- evaluateFile $ RudiFile [statement]
                runRepl $ Map.union defs newDefs
            Left _ -> do
                print $ eval defs $ parseExpr line
                runRepl defs

replFile :: String -> IO ()
replFile str = runRepl =<< loadFile str

repl = runRepl Map.empty
