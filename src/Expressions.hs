sampleInput = "deriv(x, x * y)" -- => y + x * y'
sampleInput2 = "deriv(x, x * lambda)" -- => lambda
sampleInput3 = "sin(5 + x) / (deriv(x, 2x^2))"

data Con = Con Char Char [Char] -- "lambda"
data Var = Var Char [Char] -- "x1"
data Op = Op [String]  -- "+ - *"
data Arg = Var | Con | Expr -- x1 | lambda | tbd
data Term = Var | Con [Args] -- x1 | x1 * y
data Expr = Expr Term [(Op, Term)] 

newtype Exp = Compose [Atom] deriving Eq
data Atom = Var String | Con String [Exp] deriving Eq