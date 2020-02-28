{-# LANGUAGE OverloadedStrings #-}
module Laws where

import Expressions
import Parsing

data Law = Law String Equation deriving Show
data Equation = Equation (Expr, Expr) deriving Show

addition, product, sin, cos, ln, power, constants, self :: String
addition = "addition: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
product = "product: deriv(x, a * b) = deriv(x, a) * b + deriv(x, b) * a"
sin = "sin: deriv(x, sin(a)) = deriv(x, a) * cos(a)"
cos = "cos: deriv(x, cos(a)) = deriv(x, a) * -sin(a)"
ln = "ln: deriv(x, ln(a)) = deriv(x, a) * (1 / a)"
self = "self: deriv(x, x) = 1"
power = "power: deriv(x, a ^ b) = a ^ b * deriv(x, (b * ln(a)))"
constants = "constants: deriv(x, c) = 0"

law :: Parser Law
law = do name <- upto ':'
         eqn <- equation
         return (Law name eqn)

equation :: Parser Equation
equation = do e1 <- expr;
              _ <- "="
              e2 <- expr
              return (Equation (e1, e2))
