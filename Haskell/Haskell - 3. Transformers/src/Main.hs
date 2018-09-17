module Main where

import Control.Monad.State.Lazy
import Text.Megaparsec as Meg
import Data.Map.Lazy as Map
import Control.Monad.Cont
import Control.Monad (void)
import System.IO

import ExecutorIntf
import Executor
import Parser

main :: IO ()
main = do
    putStrLn "Enter the name of file"
    fileName <- getLine
    putStrLn ("Parsing file `" ++ fileName ++ "`")

    fileDesc <- openFile fileName ReadMode
    fileContent <- hGetContents fileDesc
    let parsedTree = Meg.parse parseScriptFromSrc "" fileContent
    case parsedTree of
        Left except -> print except
        Right (Script sts) -> void $ 
            runStateT (runContT (executeStatements sts) $ const (return Normal)) Map.empty