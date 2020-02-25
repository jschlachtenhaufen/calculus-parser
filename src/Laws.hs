{-# LANGUAGE OverloadedStrings #-}
module Laws where

import Expressions
import Parsing

data Law = Law String Expr Expr deriving (Show)

addition, product, sin, cos, ln, power, constants :: String
addition = "addition: deriv(x, a + b) = deriv(x, a) + deriv(x, b)"
product = "product: deriv(x, a * b) = deriv(x, a) * b + deriv(x, b) * a"
sin = "sin: deriv(x, sin(a)) = deriv(x, a) * cos(a)"
cos = "cos: deriv(x, cos(a)) = deriv(x, a) * ..."   -- how are we dealing with negatives?
ln = "ln: deriv(x, ln(a)) = deriv(x, a) * (1 / a)"
power = "power: deriv(x, a ^ b) = a ^ b * deriv(x, (b * ln(a))"
constants = "constants: deriv(x, c) = 0" -- if c doesn't depend on x

law :: Parser Law
law = do name <- upto ':'
         e1 <- expr 
         _ <- "="
         e2 <- expr
         return (Law name e1 e2)

