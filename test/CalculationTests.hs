module CalculationTests (calculationTests) where

import Calculation
import Laws
import Expressions
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec
import Data.Maybe

-- using the laws below, this will compare the generated output of each sample input expression with its corresponding expected output Calculation
calculationTests :: TestTree
calculationTests = testGroup "Testing calculation reasoning"     
    [ testCase ("Solving '" ++ expression ++ "'") $ 
      assertEqual ("'" ++ expression ++ "' is not solved correctly") (calculate testLaws (fromJust $ parseMaybe expr expression)) (expected)
      | (expression, expected) <- sampleCases
    ]
  
-- although other laws are configurable, we will test using these for consistency:
testLaws :: [Law]
testLaws = map (fromJust . parseMaybe law) [
    "addition: deriv(x, a + b) = deriv(x, a) + deriv(x, b)",
    "subtraction: deriv(x, a - b) = deriv(x, a) - deriv(x, b)",
    "product: deriv(x, a * b) = deriv(x, a) * b + deriv(x, b) * a",
    "quotient: deriv(x, a/b) = (deriv(x, a) * b - deriv(x, b) * a) / b^2",
    "sin: deriv(x, sin(a)) = deriv(x, a) * cos(a)",
    "cos: deriv(x, cos(a)) = deriv(x, a) * -sin(a)",
    "ln: deriv(x, ln(a)) = deriv(x, a) * (1 / a)",
    "self: deriv(x, x) = 1",
    "power: deriv(x, a ^ b) = a ^ b * deriv(x, (b * ln(a)))",
    "constants: deriv(x, c) = 0"
  ]

-- list of all our sample inputs and corresponding expected outputs
sampleCases :: [(String, Calculation)]
sampleCases = [(sampleInput1, sampleOutput1), (sampleInput2, sampleOutput2), (sampleInput3, sampleOutput3), (sampleInput4, sampleOutput4), (sampleInput5, sampleOutput5)]  

sampleInput1 :: String
sampleInput1 = "deriv(x, x+y)"

-- For all of these "sampleOutputs", we walked through each step to verify that it was correctly applied.
-- You'll see the steps quickly blow up as expression complexity increases
sampleOutput1 :: Calculation
sampleOutput1 = Calc (TermFunc "deriv" [(Var "x"), (TermOp "+" (Var "x") (Var "y"))]) [
    Step "addition" (TermOp "+" (TermFunc "deriv" [(Var "x"), (Var "x")]) (TermFunc "deriv" [(Var "x"), (Var "y")])),
    Step "self" (TermOp "+" (ConstN 1) (TermFunc "deriv" [(Var "x"), (Var "y")]))
  ]

sampleInput2 :: String
sampleInput2  = "deriv(x, x^2 * sin(x))"

-- final step is: ((((x ^ 2.0) * ((0.0 * ln(x)) + ((1.0 * (1.0 / x)) * 2.0))) * sin(x)) + ((1.0 * cos(x)) * (x ^ 2.0)))
-- which simplifies to 2x(sin(x)) + x^2(cos(x)), which is correct
sampleOutput2 :: Calculation
sampleOutput2 = Calc (TermFunc "deriv" [Var "x",TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermFunc "sin" [Var "x"])]) [
    Step "product" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",TermOp "^" (Var "x") (ConstN 2.0)]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "sin" [Var "x"]]) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "sin" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",TermOp "^" (Var "x") (ConstN 2.0)]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "self" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",TermOp "^" (Var "x") (ConstN 2.0)]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "power" (TermOp "+" (TermOp "*" (TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermFunc "deriv" [Var "x",TermOp "*" (ConstN 2.0) (TermFunc "ln" [Var "x"])])) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "product" (TermOp "+" (TermOp "*" (TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "ln" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "ln" [Var "x"]]) (ConstN 2.0)))) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "ln" (TermOp "+" (TermOp "*" (TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "ln" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermOp "/" (ConstN 1.0) (Var "x"))) (ConstN 2.0)))) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "self" (TermOp "+" (TermOp "*" (TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "ln" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermOp "/" (ConstN 1.0) (Var "x"))) (ConstN 2.0)))) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0)))),
    Step "constants" (TermOp "+" (TermOp "*" (TermOp "*" (TermOp "^" (Var "x") (ConstN 2.0)) (TermOp "+" (TermOp "*" (ConstN 0.0) (TermFunc "ln" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermOp "/" (ConstN 1.0) (Var "x"))) (ConstN 2.0)))) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (TermOp "^" (Var "x") (ConstN 2.0))))
  ]

sampleInput3 :: String
sampleInput3  = "deriv(x, (lambda - sin(x)) / cos(x))"

-- final step becomes: (((0.0 - (1.0 * cos(x))) * cos(x)) - ((1.0 * -sin(x)) * (lambda - sin(x)))) / (cos(x) ^ 2.0)
-- which simplifies to (-cos^2(x) + sin(x)(lambda - sin(x))) / (cos^2(x)), which is correct
sampleOutput3 :: Calculation
sampleOutput3 = Calc (TermFunc "deriv" [Var "x",TermOp "/" (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])) (TermFunc "cos" [Var "x"])]) [
    Step "quotient" (TermOp "/" (TermOp "-" (TermOp "*" (TermFunc "deriv" [Var "x",TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])]) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "cos" [Var "x"]]) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "subtraction" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (TermFunc "deriv" [Var "x",TermFunc "lambda" []]) (TermFunc "deriv" [Var "x",TermFunc "sin" [Var "x"]])) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "cos" [Var "x"]]) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "sin" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (TermFunc "deriv" [Var "x",TermFunc "lambda" []]) (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermFunc "cos" [Var "x"]))) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "cos" [Var "x"]]) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "cos" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (TermFunc "deriv" [Var "x",TermFunc "lambda" []]) (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermFunc "cos" [Var "x"]))) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermOp "*" (ConstN (-1.0)) (TermFunc "sin" [Var "x"]))) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "self" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (TermFunc "deriv" [Var "x",TermFunc "lambda" []]) (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"]))) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermOp "*" (ConstN (-1.0)) (TermFunc "sin" [Var "x"]))) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "self" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (TermFunc "deriv" [Var "x",TermFunc "lambda" []]) (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"]))) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermOp "*" (ConstN (-1.0)) (TermFunc "sin" [Var "x"]))) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0))),
    Step "constants" (TermOp "/" (TermOp "-" (TermOp "*" (TermOp "-" (ConstN 0.0) (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"]))) (TermFunc "cos" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermOp "*" (ConstN (-1.0)) (TermFunc "sin" [Var "x"]))) (TermOp "-" (TermFunc "lambda" []) (TermFunc "sin" [Var "x"])))) (TermOp "^" (TermFunc "cos" [Var "x"]) (ConstN 2.0)))
  ]

-- test no steps
sampleInput4 :: String
sampleInput4 = "deriv(x, y)"

sampleOutput4 :: Calculation
sampleOutput4 = Calc (TermFunc "deriv" [Var "x",Var "y"]) []

sampleInput5 :: String
sampleInput5 = "deriv(x, 2x + y - 2sin(x))"

-- final step: ((((0.0 * x) + (1.0 * 2.0)) + deriv(x, y)) - ((0.0 * sin(x)) + ((1.0 * cos(x)) * 2.0)))
-- which simplifies to: 2 + deriv(x, y) - 2cos(x), which is correct
sampleOutput5 :: Calculation
sampleOutput5 = Calc (TermFunc "deriv" [Var "x",TermOp "-" (TermOp "+" (TermOp "*" (ConstN 2.0) (Var "x")) (Var "y")) (TermOp "*" (ConstN 2.0) (TermFunc "sin" [Var "x"]))]) [
    Step "subtraction" (TermOp "-" (TermFunc "deriv" [Var "x",TermOp "+" (TermOp "*" (ConstN 2.0) (Var "x")) (Var "y")]) (TermFunc "deriv" [Var "x",TermOp "*" (ConstN 2.0) (TermFunc "sin" [Var "x"])])),
    Step "addition" (TermOp "-" (TermOp "+" (TermFunc "deriv" [Var "x",TermOp "*" (ConstN 2.0) (Var "x")]) (TermFunc "deriv" [Var "x",Var "y"])) (TermFunc "deriv" [Var "x",TermOp "*" (ConstN 2.0) (TermFunc "sin" [Var "x"])])),
    Step "product" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (Var "x")) (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermFunc "deriv" [Var "x",TermOp "*" (ConstN 2.0) (TermFunc "sin" [Var "x"])])),
    Step "product" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (Var "x")) (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermFunc "deriv" [Var "x",TermFunc "sin" [Var "x"]]) (ConstN 2.0)))),
    Step "sin" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (Var "x")) (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermFunc "cos" [Var "x"])) (ConstN 2.0)))),
    Step "self" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (Var "x")) (TermOp "*" (ConstN 1.0) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (TermFunc "deriv" [Var "x",Var "x"]) (TermFunc "cos" [Var "x"])) (ConstN 2.0)))),
    Step "self" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (Var "x")) (TermOp "*" (ConstN 1.0) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (ConstN 2.0)))),
    Step "constants" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (ConstN 0.0) (Var "x")) (TermOp "*" (ConstN 1.0) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (TermFunc "deriv" [Var "x",ConstN 2.0]) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (ConstN 2.0)))),
    Step "constants" (TermOp "-" (TermOp "+" (TermOp "+" (TermOp "*" (ConstN 0.0) (Var "x")) (TermOp "*" (ConstN 1.0) (ConstN 2.0))) (TermFunc "deriv" [Var "x",Var "y"])) (TermOp "+" (TermOp "*" (ConstN 0.0) (TermFunc "sin" [Var "x"])) (TermOp "*" (TermOp "*" (ConstN 1.0) (TermFunc "cos" [Var "x"])) (ConstN 2.0))))
  ]
  
