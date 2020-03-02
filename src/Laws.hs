{-# LANGUAGE OverloadedStrings #-}
module Laws where

import Expressions
import Parsing

data Law = Law String Equation deriving Show
data Equation = Equation (Expr, Expr) deriving Show


law :: Parser Law
law = do name <- upto ':'
         eqn <- equation
         return (Law name eqn)

equation :: Parser Equation
equation = do e1 <- expr;
              _ <- "="
              e2 <- expr
              return (Equation (e1, e2))
