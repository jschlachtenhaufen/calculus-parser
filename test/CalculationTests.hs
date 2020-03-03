module CalculationTests (calculationTests) where

import Calculation
import Laws
import Expressions
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Data.Maybe

calculationTests :: TestTree
calculationTests = testGroup "Testing calculation reasoning"     
    [ testCase ("Solving '" ++ expression ++ "'") $ 
      assertEqual ("'" ++ expression ++ "' is not solved correctly") (calculate testLaws (fromJust $ parseMaybe expr expression)) (expected)
      | (expression, expected) <- sampleCases
    ]
  
-- although other laws are configurable with 'stack run', we will test using these:
testLaws :: [Law]
testLaws = map (fromJust . parseMaybe law) [
    "addition: deriv(x, a + b) = deriv(x, a) + deriv(x, b)",
    "product: deriv(x, a * b) = deriv(x, a) * b + deriv(x, b) * a",
    "sin: deriv(x, sin(a)) = deriv(x, a) * cos(a)",
    "cos: deriv(x, cos(a)) = deriv(x, a) * -sin(a)",
    "ln: deriv(x, ln(a)) = deriv(x, a) * (1 / a)",
    "self: deriv(x, x) = 1",
    "power: deriv(x, a ^ b) = a ^ b * deriv(x, (b * ln(a)))",
    "constants: deriv(x, c) = 0"
  ]

sampleCases :: [(String, Calculation)]
sampleCases = [(sampleInput1, sampleOutput1)]  

sampleInput1 :: String
sampleInput1 = "deriv(x, x+y)"

sampleOutput1 :: Calculation
sampleOutput1 = Calc (TermFunc "deriv" [(Var "x"), (TermOp "+" (Var "x") (Var "y"))]) [
    Step "addition" (TermOp "+" (TermFunc "deriv" [(Var "x"), (Var "x")]) (TermFunc "deriv" [(Var "x"), (Var "y")])),
    Step "self" (TermOp "+" (ConstN 1) (TermFunc "deriv" [(Var "x"), (Var "y")]))
  ]