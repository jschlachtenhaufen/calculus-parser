module Calculation where 

import Laws
import Expressions

data Calculation = Calc Expr [Step] deriving (Eq)
data Step = Step String Expr deriving (Eq)
type Substitution = [(String, Expr)]

calculate :: [Law] -> Expr -> Calculation
calculate laws ex = Calc ex (manyStep rws ex)
  where rws e = [ Step name e' 
                | Law name eq <- laws, 
                  e' <- rewrites (Law name eq) e,
                  e' /= e ]

manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
 = case steps of
 [] -> []
 (o@(Step _ ex) : _) -> o:manyStep rws ex
 where steps = rws e

matchExprs :: String -> Expr -> Expr -> [Substitution]

-- deriv(x, 12) = 0
matchExprs "constants" 
  (TermFunc "deriv" [(Var a), (Var b)]) 
  (TermFunc "deriv" [(Var c), (ConstN d)]) 
    = if a == c then [[(b, (ConstN d))]] else []

-- deriv(x, lambda) = 0
matchExprs "constants" 
  (TermFunc "deriv" [(Var a), (Var b)]) 
  (TermFunc "deriv" [(Var c), (TermFunc f [])]) 
    = if a == c then [[(b, (TermFunc f []))]] else []

matchExprs "constants" _ _ = []

matchExprs _ (Var a) e = [[(a, e)]]
matchExprs _ (ConstN a) (ConstN b) = if a == b then [[(show a, (ConstN b))]] else []
matchExprs _ (ConstN _) _ = []
matchExprs a (TermFunc f1 es1) (TermFunc f2 es2) 
  = if f1 == f2 then combine (zipWith (matchExprs a) es1 es2)
    else []
matchExprs _ (TermFunc _ _) _ = []

matchExprs a (TermOp op1 e1 e2) (TermOp op2 e3 e4)
  = if op1 == op2 then combine (zipWith (matchExprs a) [e1, e2] [e3, e4])
    else []
matchExprs _ (TermOp _ _ _) _ = []

rewrites :: Law -> Expr -> [Expr]
rewrites (Law name (Equation (eqL, eqR))) e 
  = [apply sub eqR | sub <- matchExprs name eqL e] ++ rewriteSubExpressions (Law name (Equation (eqL, eqR))) e

rewriteSubExpressions :: Law -> Expr -> [Expr]
rewriteSubExpressions _ (Var _) = []
rewriteSubExpressions _ (ConstN _) = []
rewriteSubExpressions l (TermFunc f es) = map (TermFunc f) (anyOne (rewrites l) es)
rewriteSubExpressions l (TermOp o e1 e2) =  [TermOp o (args!!0) (args!!1) | args <- (anyOne (rewrites l) [e1, e2])]

anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne _ [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

apply :: Substitution -> Expr -> Expr
apply sub (Var a) = find a sub
apply _ (ConstN c) = (ConstN c)
apply sub (TermFunc f es) = (TermFunc f (map (apply sub) es))
apply sub (TermOp op e1 e2) = (TermOp op (apply sub e1) (apply sub e2))

find :: String -> Substitution -> Expr
find name (((name2, e):rest)) =
  if name == name2 then e
  else find name rest
find name [] = Var name

combine :: [[Substitution]] -> [Substitution] 
combine [] = []
combine subs = (filterUnifiable . cp) subs
  where filterUnifiable = concatMap unifyAll

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

instance Show Step where
    show (Step s e) = showString s "" ++ showChar ':' "" ++ showSpace "" ++ show e

instance Show Calculation where
    show (Calc e steps) = "Expression: " ++ show e ++ "\nSTEPS:" ++ "\n\t" ++ (showSteps 1 steps)

showSteps :: Int -> [Step] -> String
showSteps i [] = show "No steps found"
showSteps i [e] = show i ++ ") " ++ show e
showSteps i (e:es) = show i ++ ") " ++ show e ++ "\n\t" ++ (showSteps (i+1) es)
showSteps _ [] = "No possible steps"