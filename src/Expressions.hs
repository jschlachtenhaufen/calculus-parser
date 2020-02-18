sampleInput = "deriv(x, x * y)"
sampleInput2 = "sin(5 + x) / (deriv(x, x^2))"

data Exp = Var String -- x,y
    | ConstN Double -- 3,5,2
    | TermFunc String [Exp] -- sin, deriv w/ args, lambda
    | TermOp String Exp Exp deriving (Eq, Show) -- -,+,*,/ with args


sampleInput1 => TermFunc "deriv" [Var "x", TermOp "*" Var "x" Var "y"]
sampleInput2 => TermOp "/" (TermFunc "sin" [TermOp "+" ConstN 5 Var "x"]) (TermFunc "deriv" [Var "x", TermOp "^" Var "x" ConstN 2])