import ExpressionTests
import CalculationTests
import LawTests

import Test.Tasty

-- First, tests the laws from laws.txt per our special feature. Then executes the static expression and calculation tests
main :: IO ()
main = do lawTests <- genLawTests
          defaultMain (testGroup "Calculus Solver Tests" [lawTests, expressionTests, calculationTests])
