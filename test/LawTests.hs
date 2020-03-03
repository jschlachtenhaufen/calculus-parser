module LawTests (genLawTests) where

import Expressions
import Laws
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.LeanCheck as LC

genLawTests :: [String] -> IO TestTree
genLawTests args = do let lawsFile = getLawsFile args
                      allLaws <- readFile lawsFile
                      let laws = filter isTestableLaw (map parseLaw (lines allLaws))
                      return (testGroup "Testing laws" [example])
          
-- either returns first arg as laws file, or default file "app/laws.txt"
getLawsFile :: [String] -> String
getLawsFile [] = "laws.txt"
getLawsFile args = args!!0

-- attempts to parse individual law, throwing error if it can't
parseLaw :: String -> Law
parseLaw l = case parse law "" l of
                 Left bundle -> error (errorBundlePretty bundle)
                 Right parsedLaw -> parsedLaw

-- true if the law only contains testable expressions
isTestableLaw :: Law -> Bool
isTestableLaw (Law _ (Equation (e1, e2))) = isTestableExpr e1 && isTestableExpr e2

-- true if the expression only contains testable functions
isTestableExpr :: Expr -> Bool
isTestableExpr (Var _) = True
isTestableExpr (ConstN _) = True
isTestableExpr (TermOp _ e1 e2) = isTestableExpr e1 && isTestableExpr e2
-- isTestableExpr (TermFunc _ []) = True
isTestableExpr (TermFunc f es) = (elem f testableFunctions) && all isTestableExpr es

testableFunctions :: [String]
testableFunctions = ["sin", "cos", "ln"]

example :: TestTree
example
  = LC.testProperty "x + y 0 = y + x"
  (\x y -> (x::Int) + (y::Int) == y + x)