module Main where

import Expressions (expr)
import Laws
import Calculation (calculate)
import Text.Megaparsec
import System.IO (hFlush, stdout)
import Control.Monad (unless)

main :: IO ()
main = do putStrLn "Parsing laws:"
          _ <- mapM putStrLn lawStrings
          putStrLn "" >> repl (parseLaws lawStrings)
          
-- error "Non negative inputs only"
lawStrings :: [String]
lawStrings = [addition, Laws.product, Laws.sin, power, ln, self, constants] -- doens't have Laws.cos cus of parsing

parseLaws :: [String] -> [Law]
parseLaws ls = map unwrap (map (parseMaybe law) ls)

unwrap :: Maybe Law -> Law
unwrap Nothing = error "failed to parse inputted law"
unwrap (Just l) = l

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
