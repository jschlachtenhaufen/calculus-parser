module Calculation where 
import Laws
import Expressions

-- datatypes
data Calculation = Calc Expr [Step] deriving (Eq)
data Step = Step String Expr deriving (Eq)
type Substitution = [(String, Expr)]

-- input: list of laws and an expression
-- output: a calcuation with steps from rewrites
calculate :: [Law] -> Expr -> Calculation
calculate laws ex = Calc ex (manyStep rws ex)
  where rws e = [ Step name e' 
                | Law name eq <- laws, 
                  e' <- rewrites (Law name eq) e,
                  e' /= e ]

-- use rws to construct as many steps as possible
manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
 = case steps of
 [] -> []
 (o@(Step _ ex) : _) -> o:manyStep rws ex
 where steps = rws e

-- input: the name of a law, an expression to subsitute and the left side of a law
-- output: a list of substitutions for the variables in the expression
matchExprs :: String -> Expr -> Expr -> [Substitution]

-- constants law works is special to handle functions with no arguments and differentiate Var and ConstN 
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

-- nothing else should match to constants law
matchExprs "constants" _ _ = []

-- apply all other laws to expr data types
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

-- input a law and an expression, apply substitutions to create a list of rewrites of the expr
rewrites :: Law -> Expr -> [Expr]
rewrites (Law name (Equation (eqL, eqR))) e 
  = [apply sub eqR | sub <- matchExprs name eqL e] ++ rewriteSubExpressions (Law name (Equation (eqL, eqR))) e

-- call rewrites on each type of Expr to create list of rewrites
rewriteSubExpressions :: Law -> Expr -> [Expr]
rewriteSubExpressions _ (Var _) = []
rewriteSubExpressions _ (ConstN _) = []
rewriteSubExpressions l (TermFunc f es) = map (TermFunc f) (anyOne (rewrites l) es)
rewriteSubExpressions l (TermOp o e1 e2) =  [TermOp o (args!!0) (args!!1) | args <- (anyOne (rewrites l) [e1, e2])]

-- function to change something of type 'a', apply it exactly once everywhere if possible
anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne _ [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

-- apply a substitution to an expression to produce a new expression
apply :: Substitution -> Expr -> Expr
apply sub (Var a) = find a sub
apply _ (ConstN c) = (ConstN c)
apply sub (TermFunc f es) = (TermFunc f (map (apply sub) es))
apply sub (TermOp op e1 e2) = (TermOp op (apply sub e1) (apply sub e2))

-- find the correct expression to substitute
find :: String -> Substitution -> Expr
find name (((name2, e):rest)) =
  if name == name2 then e
  else find name rest
find name [] = Var name

-- take all compatible combinations of substitutions
combine :: [[Substitution]] -> [Substitution] 
combine [] = []
combine subs = (filterUnifiable . cp) subs
  where filterUnifiable = concatMap unifyAll

-- cartesian product, take one element from each list
cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

-- combine substitutions into single element list if possible
unifyAll :: [Substitution] -> [Substitution]
unifyAll = foldr f [[]]
 where f sub subs = concatMap (unify sub) subs

-- combine two substitutions, if possible
unify :: Substitution -> Substitution -> [Substitution]
unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else []

-- return true if valid substitution
compatible :: Substitution -> Substitution -> Bool
compatible sub1 sub2
 = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <- sub2, v1==v2] 

-- Show instances for step and calculation
instance Show Step where
    show (Step s e) = showString s "" ++ showChar ':' "" ++ showSpace "" ++ show e

instance Show Calculation where
    show (Calc e steps) = "Expression: " ++ show e ++ "\nSTEPS:" ++ "\n\t" ++ (showSteps 1 steps)

showSteps :: Int -> [Step] -> String
showSteps _ [] = show "No steps found"
showSteps i [e] = show i ++ ") " ++ show e
showSteps i (e:es) = show i ++ ") " ++ show e ++ "\n\t" ++ (showSteps (i+1) es)