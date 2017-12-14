module Rudi.Repl
    (
        repl,
        replFile,
        loadFile
    ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils (replace)

import Text.ParserCombinators.Parsec

import Rudi.Interpreter
import Rudi.Parser
import Rudi.Types

-- Transforms the path in an import statement (e.g. "Prelude.std") into a path (e.g., "Prelude/std.rudi")
-- Specifically, will append ".rudi" to the end, replace '.' with '/', and make "super" into ".."
createPath :: String -> String
createPath str = replace "super" ".." (replace "." "/" str) ++ ".rudi"

evaluateFile :: RudiFile -> IO (Map Expr Expr)
evaluateFile (RudiFile []) = return Map.empty
evaluateFile (RudiFile (EmptyStatement:statements)) = evaluateFile (RudiFile statements)
evaluateFile (RudiFile (Import path:statements)) = do
    m <- evaluateFile $ RudiFile statements

    loadedMap <- loadFile $ createPath path

    return $ Map.union m loadedMap

evaluateFile (RudiFile (def@(Define x y):statements)) = do
    m <- evaluateFile $ RudiFile statements

    case compile def of
        Define x y -> return $ Map.insert x y m
        _ -> return m

loadFile :: String -> IO (Map Expr Expr)
loadFile path = do
    contents <- readFile path
    evaluateFile $ parseFile contents

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
