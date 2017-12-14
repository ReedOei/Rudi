module Main where

import System.Environment

import Rudi.Main

main :: IO ()
main = do
    args <- getArgs

    case args of
        [fname] -> replFile fname
        _ -> repl

