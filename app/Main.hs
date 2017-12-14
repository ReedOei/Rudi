module Main where

import System.Environment
import System.IO

import Rudi.Main

main :: IO ()
main = do
    args <- getArgs
    hSetBuffering stdout NoBuffering

    case args of
        [fname] -> replFile fname
        _ -> repl

