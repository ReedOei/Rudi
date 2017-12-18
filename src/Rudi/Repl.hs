module Rudi.Repl
    (
        repl,
        replFile,
        loadFile
    ) where

import Control.Monad.IO.Class

import Data.Map (Map)
import qualified Data.Map as Map
import Data.String.Utils (replace)

import System.Console.Haskeline

import Text.ParserCombinators.Parsec

import Rudi.Interpreter
import Rudi.Parser
import Rudi.Types

-- Transforms the path in an import statement (e.g. "Prelude.std") into a path (e.g., "Prelude/std.rudi")
-- Specifically, will append ".rudi" to the end, replace '.' with '/', and make "super" into ".."
createPath :: String -> String
createPath str = replace "super" ".." (replace "." "/" str) ++ ".rudi"

evaluateFile :: Map Expr Expr -> RudiFile -> IO (Map Expr Expr)
evaluateFile m (RudiFile []) = return m
evaluateFile m (RudiFile (InternalCommand command commandVal:statements)) = do
    case command of
        ShowDef ->
            case getDefinition commandVal m of
                Nothing -> do
                    putStrLn $ "No definition called '" ++ commandVal ++ "'"
                    putStrLn "Valid definitions are: "

                    print $ Map.keys m
                Just def -> print def

    evaluateFile m $ RudiFile statements

evaluateFile m (RudiFile (EmptyStatement:statements)) = evaluateFile m (RudiFile statements)
evaluateFile m (RudiFile (Import path:statements)) = do
    loadedMap <- loadFile m $ createPath path

    evaluateFile (Map.union loadedMap m) $ RudiFile statements

evaluateFile m (RudiFile (def@(Define x y):statements)) =
    case compile m def of
        Define x y -> evaluateFile (Map.insert x y m) $ RudiFile statements
        _ -> return m

loadFile :: Map Expr Expr -> String -> IO (Map Expr Expr)
loadFile m path = do
    contents <- readFile path
    evaluateFile m $ parseFile contents

runRepl :: Map Expr Expr -> InputT IO ()
runRepl defs = do
    input <- getInputLine "> "

    case input of
        Nothing -> runRepl defs
        Just line ->
            if unwords (words line) == "" then
                runRepl defs
            else
                case parse rudiStatement "" line of
                    Right statement -> do
                        newDefs <- liftIO $ evaluateFile defs $ RudiFile [statement]
                        runRepl $ Map.union defs newDefs
                    Left _ -> do
                        outputStrLn $ show $ eval defs $ parseExpr line
                        runRepl defs

replFile :: String -> IO ()
replFile str = do
    defs <- loadFile Map.empty str
    runInputT defaultSettings $ runRepl defs

repl = runInputT defaultSettings $ runRepl Map.empty

