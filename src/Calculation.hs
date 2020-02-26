module Calculation where

import Text.Megaparsec
import Laws
import Expressions

data Calculation = Calc Expr [Step] deriving Show
data Step = Step String Expr deriving Show

input = ""

addLaw = parseMaybe law addition
expr1 = parseMaybe expr sampleInput1
ex2 = TermFunc "sin" [Var "x",TermOp "+" (Var "a") (Var "b")]

c1 :: Maybe Law -> Maybe Expr -> Calculation
c1 Nothing _ = undefined
c1 _ Nothing = undefined
c1 (Just l) (Just e) = calculate [l] e

calculate :: [Law] -> Expr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where rws e = [ Step name e' 
                | Law name eq <- laws, 
                  e' <- rewrites eq e,
                  e' /= e ]


manyStep :: (Expr -> [Step]) -> Expr -> [Step]
manyStep rws e
 = case steps of
 [] -> []
 (o@(Step _ e) : _) -> o:manyStep rws e
 where steps = rws e

anyOne :: (a -> [a]) -> [a] -> [[a]] 
anyOne f [] = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++
                  [x:xs' | xs' <- anyOne f xs]

-- splits :: [a] -> [([a],[a])]
-- splits [] = [([],[])] 
-- splits (a:as) = [([],a:as)] ++ [(a:as1,as2) | (as1,as2) <- splits as]
-- splits as = [(take n as,drop n as) | n <- [0..length as]]

-- splits3 as = [(as1,as2,as3) | (as1,bs) <- splits as, (as2,as3) <- splits bs] 


{-
  eqn: deriv(x, sin(a)) = deriv(x, a) * cos(a)
  expression: 4
-}


rewrites eqn as = rewritesSeg eqn as ++ anyOne (rewritesA eqn) as



-- outer :: Expr -> [Expr]
-- outer a = a : possibleSubexpressions a

-- possibleSubexpressions :: Expr -> [Expr]
-- possibleSubexpressions (Var _) = []
-- possibleSubexpressions (ConstN _) = []
-- possibleSubexpressions (TermFunc func exprs) = concatMap outer exprs
-- possibleSubexpressions (TermOp op e1 e2) = outer e1 ++ outer e2

-- rewrites :: Eqution -> Expr -> [Expr]

-- rewrites :: Equation -> Expr -> [Expr] 
-- -- rewrites eqn (e)
-- --  = map Compose (rewriteSeg eqn e ++ anyOne (rewritesA eqn) e)
-- rewrites eqn (Var _) = []
-- rewrites eqn (ConstN _) = []
-- -- rewrites eqn (TermFunc func exprs) = map (TermFunc func) (anyOne (rewrites eqn) exprs) 
-- -- rewrites eqn (TermOp op e1 e2) = [e2]

-- -- rewritesSeg :: Equation -> [Atom] -> [[Atom]]
-- rewritesSeg :: Equation -> Expr -> [Expr]
-- rewritesSeg (e1, e2) as
--  = [as1 ++ (apply subst e2) ++ as3
--  | (as1,as2,as3) <- split3 as
--  , subst <- match (e1, Compose as2) ] 
  
-- -- rewritesSeg :: Equation -> [Atom] -> [[Atom]]
-- -- rewritesSeg (e1,e2) as
-- --   = [as1 ++ deCompose (apply sub e2) ++ as3
-- --     | (as1,as2,as3) <- segments as,
-- --     sub <- match (e1,Compose as2)]

-- parts :: Int -> [a] -> [[[a]]]
-- parts 0 [] = [[]]
-- parts 0 as = []
-- parts n as = [bs:bss
-- | (bs,cs) <- splits as,
-- bss <- parts (n-1) cs]

-- alignments :: (Expr, Expr) -> [([Atom],Expr)]
-- alignments (Compose as, Compose bs)
--  = [zip as (map Compose bss) | bss <- splitsN (length as) bs]

-- match :: (Expr, Expr) -> [Subst]
-- match = concatMap (combine . map matchA) . alignments

-- matchA :: (Atom, Expr) -> [Subst]
-- matchA (Var v, e) = [unitSub v e]
-- matchA (Con k1 es1, Compose [Con k2 es2]) | k1 ==k2
--  = combine (map match (zip es1 es2))

-- type Subst = [(VarName,Expr)]
-- type VarName = String
-- unitSub :: VarName -> Expr -> Subst
-- unitSub v e = [(v,e)]

-- apply :: Subst -> Expr -> Expr
-- apply sub (Compose as) = Compose (concatMap (applyA sub) as)
-- applyA :: Subst -> Atom -> [Atom]
-- applyA sub (Var v) = deCompose (binding sub v)
-- applyA sub (Con k es) = [Con k (map (apply sub) es)]

-- binding :: Subst -> VarName -> Expr
-- binding ((v’,e):sub) v | v’ == v = e
--  | otherwise = binding sub v
-- binding [] v = error “Could not find binding”

-- combine :: [[Subst]] -> [Subst] 
-- combine = filterUnifiable . cp
-- filterUnifiable = concatMap unifyAll
-- unifyAll :: [Subst] -> [Subst] 

-- cp :: [[a]] -> [[a]], cp [] = [[]]
-- cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]

-- unifyAll :: [Subst] -> [Subst]
-- unifyAll = foldr f e
--  where f sub subs = concatMap (unify sub) subs

-- unify :: Subst -> Subst -> [Subst]
-- unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else []

-- compatible :: Subst -> Subst -> Bool
-- compatible sub1 sub2
--  = and [e1 == e2 | (v1, e1) <- sub1, (v2, e2) <-sub, v1==v2) 


-- testEqn = Equation (Var "x", Var "y")