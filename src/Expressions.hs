module Expressions where

sampleInput = "deriv(x, x * y)"
sampleInput2 = "sin(5 + x) / (deriv(x, x^2))"

data Expr = 
      Var String -- x,y
    | ConstN Double -- 3,5,2
    | TermFunc String [Expr] -- sin, deriv w/ args, lambda
    | TermOp String Expr Expr deriving (Eq, Show) -- -,+,*,/ with args