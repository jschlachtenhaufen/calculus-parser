import ExpressionTests
import CalculationTests
import LawTests

import System.Environment
import Test.Tasty

main :: IO ()
main = do args <- getArgs
          lawTests <- genLawTests args
          defaultMain (testGroup "Calculus Solver Tests" [lawTests, expressionTests, calculationTests])
