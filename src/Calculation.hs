module Calculation where 

import Text.Megaparsec
import Laws
import Expressions


sampleInput1, sampleInput2 :: String
sampleInput1 = "deriv(y, y + f)"
sampleInput2 = "sin(5 + x) / (deriv(x, x^2))"

data Calculation = Calc Expr [Step] deriving Show
data Step = Step String Expr deriving Show
type Substitution = [(String, Expr)]

testExpr = TermFunc "deriv" [Var "x",TermOp "+" (Var "a") (Var "b")]
testExpr2 = TermFunc "deriv" [Var "y",TermOp "+" (TermOp "+" (Var "a") (Var "b")) (Var "c")]
testLaw = Law "addition" (Equation (TermFunc "deriv" [Var "x",TermOp "+" (Var "a") (Var "b")],TermOp "+" (TermFunc "deriv" [Var "x",Var "a"]) (TermFunc "deriv" [Var "x",Var "b"])))
testEqn = Equation (TermFunc "deriv" [Var "x",TermOp "+" (Var "a") (Var "b")],TermOp "+" (TermFunc "deriv" [Var "x",Var "a"]) (TermFunc "deriv" [Var "x",Var "b"]))

addLaw = parseMaybe law addition
expr1 = parseMaybe expr sampleInput1

c1 :: Maybe Law -> Maybe Expr -> Calculation
c1 Nothing _ = undefined
c1 _ Nothing = undefined
c1 (Just l) (Just e) = calculate [l] e

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where rws e = [ Step name e' 
                | Law name eq <- laws, 
                  e' <- rewrites (Law name eq) e,
                  e' /= e ]

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
 = case steps of
 [] -> []
 (o@(Step _ e) : _) -> o:manyStep rws e
 where steps = rws e

matchExprs :: String -> Expr -> Expr -> [Substitution]
matchExprs "constants" (Var a) (ConstN c) = [[(a, (ConstN c))]]
matchExprs "constants" (Var a) (TermFunc func []) = [[(a, (TermFunc func []))]]


matchExprs _ (Var a) e = [[(a, e)]]
matchExprs _ (ConstN _) _ = []
matchExprs a (TermFunc f1 es1) (TermFunc f2 es2) 
  = if f1 == f2 then combine (zipWith (matchExprs a) es1 es2)
    else []
matchExprs _ (TermFunc f1 es) _ = []

matchExprs a (TermOp op1 e1 e2) (TermOp op2 e3 e4)
  = if op1 == op2 then combine (zipWith (matchExprs a) [e1, e2] [e3, e4])
    else []
matchExprs _ (TermOp op e1 e2) _ = []


rewrites :: Law -> Expr -> [Expr]
rewrites (Law name (Equation (eqL, eqR))) e = [apply sub eqR | sub <- matchExprs name eqL e] ++ rewriteSubExpressions (Law name (Equation (eqL, eqR))) e

rewriteSubExpressions :: Law -> Expr -> [Expr]
rewriteSubExpressions _ (Var _) = []
rewriteSubExpressions _ (ConstN _) = []
rewriteSubExpressions law (TermFunc func es) = map (TermFunc func) (anyOne (rewrites law) es)
rewriteSubExpressions law (TermOp op e1 e2) =  [TermOp op (args!!0) (args!!1) | args <- (anyOne (rewrites law) [e1, e2])]

anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

apply :: Substitution -> Expr -> Expr
apply sub (Var a) = find a sub
apply sub (ConstN c) = (ConstN c)
apply sub (TermFunc func es) = (TermFunc func (map (apply sub) es))
apply sub (TermOp op e1 e2) = (TermOp op (apply sub e1) (apply sub e2))

find :: String -> Substitution -> Expr
find name (((name2, e):tail)) =
  if name == name2 then e
  else find name tail
find name [] = Var name

combine :: [[Substitution]] -> [Substitution] 
combine = (filterUnifiable . cp)

filterUnifiable = concatMap unifyAll

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

unifyAll :: [Substitution] -> [Substitution]
unifyAll = foldr f [[]]
 where f sub subs = concatMap (unify sub) subs

unify :: Substitution -> Substitution -> [Substitution]
unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else []

compatible :: Substitution -> Substitution -> Bool
compatible sub1 sub2
 = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <- sub2, v1==v2] 
