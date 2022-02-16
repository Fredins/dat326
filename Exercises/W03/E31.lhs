exercise 3.1

\begin{code}
{-# LANGUAGE TupleSections #-}
import Test.QuickCheck (quickCheck)
import Debug.Trace (trace)

type R = Double 
type F1 = R -> R
type F2 = (R, R) -> R

-- f is continous and defined for f(a).
-- lim is defined for R \ {0}. 
-- lim of a=0 is the smallest element in the set
lim :: R -> F1 -> R
lim a f | a /= 0    = f a
        | otherwise = f 1e-5

psi :: F1 -> (R -> R -> R)
psi f x h = (f (x + h) - f x) / h

d :: F1 -> F1
d f = lim 0 . psi f 

psi1 :: F2 -> (R,R) -> R -> R
psi1 f (x, y) h = (f (x + h, y) - f (x, y)) / h

psi2 :: F2 -> (R,R) -> R -> R
psi2 f (x, y) h = (f (x, y + h) - f (x, y + h)) / h

d1, d2 :: F2 -> F2        
d1 f (a, b) = d (f . sndFixed b) a
d2 f (a, b) = d (f . fstFixed a) b

fstFixed :: a -> (b -> (a, b))
fstFixed a = (a,)

sndFixed :: b -> (a -> (a, b))
sndFixed b = (,b)

prop_partial_d :: (R, R) -> Bool
prop_partial_d x =  d1 f x =:= d1f (fst x) 
                 && d2 f x =:= d2f (snd x)
  where 
  (=:=) y z = abs (y - z) < 1e-1

f :: F2
f   (a, b) = 4*a^3 + b^2 + 1
d1f, d2f :: F1
d1f  a      = 12*a^2  
d2f  b      = 2*b^1

\end{code}

