import ExpressionTests
import CalculationTests
import LawTests

import System.Environment
import Test.Tasty

main :: IO ()
main = do lawTests <- genLawTests
          defaultMain (testGroup "Calculus Solver Tests" [lawTests, expressionTests, calculationTests])
