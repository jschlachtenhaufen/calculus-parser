module Main where

import Expressions (expr)
import Laws
import Calculation (calculate)
import Text.Megaparsec
import System.Environment
import System.IO (hFlush, stdout)
import Control.Monad (unless)

main :: IO ()
main = do args <- getArgs
          let lawsFile = getLawsFile args
          allLaws <- readFile lawsFile
          let lawList = lines allLaws
          putStrLn "Parsing laws:"
          let laws = map parseLaw lawList
          mapM_ putStrLn (map show laws)
          putStrLn "" >> repl laws
          

-- either returns first arg as laws file, or default file "app/laws.txt"
getLawsFile :: [String] -> String
getLawsFile [] = "app/laws.txt"
getLawsFile args = args!!0

-- attempts to parse individual law, throwing error if it can't
parseLaw :: String -> Law
parseLaw l = case parse law "" l of
                 Left bundle -> error (errorBundlePretty bundle)
                 Right parsedLaw -> parsedLaw

repl :: [Law] -> IO ()
repl laws = do
  input <- read_
  unless (input == ":quit")
       $ (eval_ laws input) >> repl laws

read_ :: IO String
read_ = putStr "Enter calculus expression: "
     >> hFlush stdout
     >> getLine

eval_ :: [Law] -> String -> IO ()
eval_ laws input = case parse expr "" input of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right expression -> putStrLn (show (calculate laws expression))
