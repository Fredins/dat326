PART 1

data FunExp  =  Const REAL
             |  X
             |  FunExp :+: FunExp
             |  FunExp :*: FunExp
             |  Recip FunExp
             |  Negate FunExp
             |  Exp FunExp
             |  Sin FunExp
             |  Cos FunExp

\begin{code}
{-# LANGUAGE FlexibleInstances #-}
import Prelude hiding ((+), (*))
import qualified Prelude 
import DSLsofMath.FunExp hiding (derive)
import DSLsofMath.Derive 
import DSLsofMath.FunNumInst
import DSLsofMath.Algebra
--import DSLsofMath.FunExpClass
--import DSLsofMath.FunExpInst

type FunSem = REAL -> REAL
eval'' :: FunExp -> FunSem
eval'' = eval . derive . derive

p :: (FunExp -> FunSem) -> Bool 
p h = undefined

h0 :: Eq b => (a -> b, a, b) -> Bool
h0 (h, a, b) = h a == b
-- forall x
h1 :: Eq b => (a -> b, a -> a, b -> b) -> a -> Bool
h1 (h, f, g) x = h (f x) == g (h x)
-- forall x, y
h2 :: Eq b => (a -> b, a -> a -> a, b -> b -> b) -> a -> a -> Bool
h2 (h, op1, op2) x y = h (op1 x y) == op2 (h x) (h y)

-- 7 is an arbitrary number
instance Eq FunSem where
  (==) f g = f 7 == g 7 
  (/=) f = not . (==) f

counterExamplePart1 = h2(eval'', (:*:), (*)) (X :+: Const 1) X 

\end{code}

P(eval'') = Exist Ci, ci, i:{0,1,2}. ANDi hi(eval'', C, c) 
where Ci and ci are functions of FunExp and FunSem respectively, with arity i.
hi is defined and above and needs to hold for all x and y of type FunExp.

not P (eval'')                                       =
Forall Ci, ci, i:{0,1,2}. ORi not hi(eval'', Ci, ci) =
Exist Ci, ci, i:{0,1,2}. not hi (eval'', Ci, ci)          

A counter example which disproves P(eval'') is h(eval'', :*:, *), 
which is not true for x,y:FunExp x = (X :*: Const 1), y = X. 

h2(eval'', (:*:), (*)) (X :*: Const 1) X

VL                             = 
eval'' ((X :+: Const 1) :*: X) = { equality for FunExp }
eval'' (X :*: X :+: X)         = { perform the double derivation } 
2                          
HL                                   =
eval'' (X :+: Const 1)  :*: eval'' X = { perform both of the double derivations }
1 :*: 0                              = { monoid rule x * 0 = 0 }
0                              
VL /= HL !





PART 2

\begin{code}




\end{code}





