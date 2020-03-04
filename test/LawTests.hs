module LawTests (genLawTests) where

import Expressions
import Calculation
import Laws
import Text.Megaparsec
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.LeanCheck as LC

genLawTests :: [String] -> IO TestTree
genLawTests args = do let lawsFile = getLawsFile args
                      allLaws <- readFile lawsFile
                      let laws = filter isTestableLaw (map parseLaw (lines allLaws))
                      putStrLn "Parsing inputted laws:"
                      mapM_ putStrLn (map show laws)
                      return (testGroup "Testing laws" (map createTestCase laws))
          
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
isTestableExpr (TermFunc f es) = (elem f testableFunctions) && all isTestableExpr es

-- only functions we know how to evaluate are sin, cos, ln
testableFunctions :: [String]
testableFunctions = ["sin", "cos", "ln"]

-- creates the test case for a given law using lean check
createTestCase :: Law -> TestTree
createTestCase l@(Law name (Equation (e1, e2))) = testCase (show l) $ sequence_ (
    [ assertEqual ("'" ++ name ++ "' law failed with substitution: " ++ show sub) (evaluate (apply sub e1)) (evaluate (apply sub e2)) 
      | sub <- createSubs (findFreeVariables (e1))
    ]
  )

-- returns a list of all unique free variables in an expression
findFreeVariables :: Expr -> [String]
findFreeVariables _ = ["a", "b", "c"] -- TODO for real

-- creates subtitutions for each free variable with a double from lean check
createSubs :: [String] -> [Substitution]
createSubs freeVars = [[ (v, (ConstN (d::Double))) | v <- freeVars ] | d <- take numSamples LC.list, not (isInfinite d)]
  where numSamples = quot 200 (length freeVars)

-- evaluates the expression to return a double
evaluate :: Expr -> Double
evaluate (ConstN c) = c
evaluate (Var a) = error ("No substitutions for free variable '" ++ a ++ "'")
evaluate (TermOp "+" e1 e2) = evaluate e1 + evaluate e2
evaluate (TermOp "-" e1 e2) = evaluate e1 - evaluate e2
evaluate (TermOp "*" e1 e2) = evaluate e1 * evaluate e2
evaluate (TermOp "/" e1 e2) = evaluate e1 / evaluate e2
evaluate (TermOp "^" e1 e2) = evaluate e1 ** evaluate e2
evaluate (TermFunc "sin" [arg]) = sin (evaluate arg)
evaluate (TermFunc "cos" [arg]) = cos (evaluate arg)
evaluate (TermFunc "ln" [arg]) = log (evaluate arg)
evaluate (TermFunc f _) = error ("'" ++ f ++ "' takes exactly one argument")
evaluate e = error ("unexpected expression: '" ++ show e ++ "'")