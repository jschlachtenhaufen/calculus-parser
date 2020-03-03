module ExpressionTests (expressionTests) where

import Expressions
import Test.Tasty
import Test.Tasty.HUnit
import Text.Megaparsec

expressionTests :: TestTree
expressionTests = testGroup "Testing expression parsing" [validExpressions, invalidExpressions]

validExpressionCases :: [(String, Expr)]
validExpressionCases = [
    ("x", Var "x"),
    ("x123", Var "x123"),
    ("10", ConstN 10),
    ("0", ConstN 0),
    -- ("1.54", ConstN 1.54)
    ("lambda", TermFunc "lambda" []),
    ("sum2", TermFunc "sum2" []),
    ("a*b", TermOp "*" (Var "a") (Var "b")),
    ("a + b *c^ d", TermOp "+" (Var "a") (TermOp "*" (Var "b") (TermOp "^" (Var "c") (Var "d")))),
    ("deriv(x, x+y)", TermFunc "deriv" [(Var "x"), (TermOp "+" (Var "x") (Var "y"))]),
    ("sin(lambda)", TermFunc "sin" [(TermFunc "lambda" [])]),
    ("-x", TermOp "*" (ConstN (-1)) (Var "x"))
  ]

validExpressions :: TestTree
validExpressions = testGroup "Testing valid expressions" 
    [ testCase ("Parsing '" ++ expression ++ "'") $ 
      assertEqual ("'" ++ expression ++ "' does not parse correctly") (parseMaybe expr expression) (Just expected)
      | (expression, expected) <- validExpressionCases
    ]

invalidExpressionCases :: [String]
invalidExpressionCases = [
    "sin(,)",
    "x1a",
    "sin(x",
    "deriv(x, y, z",
    "+xy",
    "10 * ^ 4",
    "deriv(x y z)"
  ]

invalidExpressions :: TestTree
invalidExpressions = testGroup "Testing invalid expressions" 
    [ testCase ("Parsing '" ++ expression ++ "'") $ 
      assertEqual ("'" ++ expression ++ "' should not be parsable") (parseMaybe expr expression) (Nothing)
      | expression <- invalidExpressionCases
    ]