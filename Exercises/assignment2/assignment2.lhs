\begin{code}
{-# LANGUAGE  FlexibleContexts, FlexibleInstances #-}
import Prelude hiding (  (+), (-), (*), (/), negate, recip, (^),
                         pi, sin, cos, exp, fromInteger, fromRational)
import qualified Prelude 
import DSLsofMath.FunExp hiding (derive)
import DSLsofMath.Derive 
import DSLsofMath.FunNumInst
import DSLsofMath.Algebra
import Test.QuickCheck (quickCheck, Arbitrary, arbitrary, elements, Gen, sized, frequency, sample)
\end{code}

PART 1a
\begin{code}
type FunSem = REAL -> REAL
eval'' :: FunExp -> FunSem
eval'' = eval . derive . derive

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

PART 1 b
\begin{code}
type Tri a     = (a, a, a)
type TriFun a  = Tri (a->a)   -- = |(a->a, a->a, a->a)|
type FunTri a  = a -> Tri a   -- = |a -> (a, a, a)|

instance Additive a => Additive (Tri a) where
  (+)  = addTri;   zero  = zeroTri
instance (Additive a, Multiplicative a) => Multiplicative (Tri a) where
  (*)  = mulTri;   one   = oneTri

instance AddGroup a => AddGroup (Tri a) where
  negate  = negateTri
instance (AddGroup a, MulGroup a) => MulGroup (Tri a) where
  recip   = recipTri

three0 f = (f, f, f)
three1 f (x, y, z) = (f x, f y, f z)
three2 f (x, y, z) (x1, y1, z1) = (f x x1, f y y1, f z z1)

addTri    :: Additive a => Tri a -> Tri a -> Tri a
addTri     = three2 (+)
zeroTri   :: Additive a => Tri a
zeroTri    = three0 zero
mulTri    :: Multiplicative a => Tri a -> Tri a -> Tri a
mulTri     = three2 (*) 
oneTri    :: Multiplicative a => Tri a
oneTri     = three0 one
negateTri :: AddGroup a => Tri a -> Tri a
negateTri  = three1 negate
recipTri  :: (AddGroup a, MulGroup a) => Tri a -> Tri a
recipTri   = three1 recip

instance Transcendental a  => Transcendental (Tri a)  where
  pi = piTri;   sin = sinTri;   cos = cosTri;   exp = expTri

piTri  :: Transcendental a => Tri a
piTri   = three0 pi 
sinTri :: Transcendental a => Tri a -> Tri a
sinTri  = three1 sin
cosTri :: Transcendental a => Tri a -> Tri a
cosTri  = three1 cos
expTri :: Transcendental a => Tri a -> Tri a
expTri  = three1 exp


-- manage floating point equality
infix 4 ~=
(~=) :: (AddGroup a, Transcendental a, Ord a) => Tri a -> Tri a -> Bool
(~=) a b = a - b < exp (fromInteger (-5))

prop_inst :: (Multiplicative a, Transcendental a, Ord a) => Tri a -> Bool
prop_inst a = sin a ^ 2 + cos a ^ 2 ~= one
\end{code}

Part 1c
\begin{code}

conTri :: a -> Tri a
conTri x = (x, x, x)

evalDD :: Transcendental a => FunExp -> a -> Tri a
evalDD (Const alpha)  =  const (fromRational (toRational alpha))
evalDD X              =  conTri 
evalDD (e1 :+: e2)    =  evalDD e1 + evalDD e2
evalDD (e1 :*: e2)    =  evalDD e1 * evalDD e2
evalDD (Recip e)      =  recip (evalDD e)
evalDD (Negate e)     =  negate (evalDD e)
evalDD (Exp e)        =  exp (evalDD e)      
evalDD (Sin e)        =  sin (evalDD e)
evalDD (Cos e)        =  cos (evalDD e)

instance Arbitrary FunExp where
  arbitrary = sized gen
    where 
    gen :: Int -> Gen FunExp
    gen 0 = elements [const X, Const] >>= \x -> x . fromInteger <$> arbitrary -- decimals are ugly
    gen 1 = sequence [gen 0, elements [Recip, Negate, Exp, Sin, Cos] >>= \x -> x <$> gen 0] >>= \xs -> elements xs
    gen n = frequency [(2, gen 1), (1, g)]
      where 
      g = do 
        x <- gen (n - 1)
        y <- gen (n - 2)
        op <- elements [(:*:), (:+:)]
        return (op x y)

testGenFunExp = sample (arbitrary :: Gen FunExp)

prop_evalDD :: (Transcendental a, Eq a) => FunExp -> a -> Bool
prop_evalDD a x = evalDD a x == conTri (eval a x)

\end{code}

Part 1d

h2 (evalDD, (:*:), (*)) x y = evalDD ((:*:) x y) == (*) (evalDD x) (evalDD y)

evalDD ((:*:) x y)        = { infix op.  } 
evalDD (f :*: g)          = { def. evalDD for :*: }
evalDD f * evalDD g       = { infix op.  } 
(*) (evalDD x) (evalDD y)

Part 2
\begin{code}
lim :: R -> (R -> R) -> R
lim a f | a /= 0    = f a
        | otherwise = f 1e-5

psi :: (R -> R) -> (R -> R -> R)
psi f x h = (f (x + h) - f x) / h

d :: (R -> R) -> (R -> R)
d f = lim 0 . psi f 

infix 4 =:=, =/=
(=:=), (=/=) :: R -> R -> Bool
(=:=) a b = not (a =/= b)
(=/=) a b = a Prelude.- b > 0.1

type R = REAL
newton :: (R -> R) -> R -> R -> R
newton f e x | abs fx < e    =  x
             | fx' =/= zero  = newton f e next 
             | otherwise     = newton f e (x + e)
  where 
  fx  = f x
  fx' = d f x
  next = x - (fx / fx')

test0 :: MulGroup a => a -> a
test0  = (^2)                      -- roots x=0 (double)
test1 :: Field a => a -> a
test1 x = x^2 - one                -- roots x=-1 x=1
test2 :: Transcendental a => a -> a
test2  = sin                        -- roots x=pi*n, n:N
test3 n x y = y^n - conTri x

newtonTri :: (Tri R -> Tri R) -> R -> R -> R
newtonTri f e x = newton (reduce . f . conTri) e x
  where 
  reduce :: Tri R -> R  
  reduce a@(x,_,_) | a == conTri x = x 
                   | otherwise       = error "newtonTri: tuple elements not same"

roots nt f = map (nt f 0.001) 

prop_newton =  all (uncurry (=:=)) (zip (f0 newton) (f0 newtonTri))
            && all (uncurry (=:=)) (zip (f1 newton) (f1 newtonTri))
            && all (uncurry (=:=)) (zip (f2 newton) (f2 newtonTri))
            && all (uncurry (=:=)) (zip (f0 newtonTri) (replicate 8 0))
 where 
 f0 nt = roots nt test0 [-2.0, -1.5 .. 2]
 f1 nt = roots nt test1 [-2.0, -1.5 .. 2]
 f2 nt = roots nt test2 [-1, 3, 6]

\end{code}

Part 3
\begin{code}
-- TODO 
\end{code}
