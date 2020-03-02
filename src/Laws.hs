{-# LANGUAGE OverloadedStrings #-}
module Laws where

import Expressions
import Parsing

data Law = Law String Equation
data Equation = Equation (Expr, Expr)


law :: Parser Law
law = do name <- upto ':'
         eqn <- equation
         return (Law name eqn)

equation :: Parser Equation
equation = do e1 <- expr;
              _ <- "="
              e2 <- expr
              return (Equation (e1, e2))


instance Show Law where
    show (Law s eq) = showString s "" ++ showChar ':' "" ++ showSpace "" ++ show eq

instance Show Equation where
    show (Equation (e1, e2)) = show e1 ++ showSpace "" ++ showChar '=' "" ++ showSpace "" ++ show e2

