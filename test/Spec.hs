import ExpressionTests
import CalculationTests

import Test.Tasty

main :: IO ()
main = do defaultMain (testGroup "Calculus Solver Tests" [expressionTests, calculationTests])
