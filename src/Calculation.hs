module Calculation where

import Laws
import Expressions

data Calculation = Calc Expr [Step]
data Step = Step String Expr

-- calculate :: [Law] -> Expr -> Calculation
-- calculate laws e = Calc e (manyStep rws e)
--   where rws e = [ Step name e' 
--                 | Law names eq <- sortedLaws, 
--                   e' <- rewrites eqn e,
--                   e' /= e ]
