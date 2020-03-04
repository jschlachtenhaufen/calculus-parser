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
    ("15", ConstN 15),
    ("1.54", ConstN 1.54),
    ("-22.81", TermOp "*" (ConstN (-1)) (ConstN 22.81)),
    ("lambda", TermFunc "lambda" []),
    ("sum2", TermFunc "sum2" []),
    ("func^ 12", TermOp "^" (TermFunc "func" []) (ConstN 12)),
    ("a*b", TermOp "*" (Var "a") (Var "b")),
    ("a + b *c^ d", TermOp "+" (Var "a") (TermOp "*" (Var "b") (TermOp "^" (Var "c") (Var "d")))),
    ("deriv(x, x+y)", TermFunc "deriv" [(Var "x"), (TermOp "+" (Var "x") (Var "y"))]),
    ("sin(lambda)", TermFunc "sin" [(TermFunc "lambda" [])]),
    ("-x", TermOp "*" (ConstN (-1)) (Var "x")),
    ("2sin(x)", TermOp "*" (ConstN 2) (TermFunc "sin" [Var "x"])),
    ("4pi", TermOp "*" (ConstN 4) (TermFunc "pi" [])),
    ("-4alpha", TermOp "*" (ConstN (-1)) (TermOp "*" (ConstN 4) (TermFunc "alpha" []))),
    ("-1.5ln(z)", TermOp "*" (ConstN (-1)) (TermOp "*" (ConstN 1.5) (TermFunc "ln" [Var "z"]))),
    ("x ^ -1.5ln(z)", TermOp "^" (Var "x") (TermOp "*" (ConstN (-1)) (TermOp "*" (ConstN 1.5) (TermFunc "ln" [Var "z"])))),
    ("a(b/4)", TermOp "*" (Var "a") (TermOp "/" (Var "b") (ConstN 4)))
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
    "deriv(x y z)",
    "2 sin(x)",
    "-+x",
    ")14",
    "ln(,x)"
  ]

invalidExpressions :: TestTree
invalidExpressions = testGroup "Testing invalid expressions" 
    [ testCase ("Parsing '" ++ expression ++ "'") $ 
      assertEqual ("'" ++ expression ++ "' should not be parsable") (parseMaybe expr expression) (Nothing)
      | expression <- invalidExpressionCases
    ]