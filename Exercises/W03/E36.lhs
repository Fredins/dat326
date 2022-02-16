
L (t, q, v) = m * v^2/2 - m * g * q 

d/dt * dL/dq' - dL/dq = 0


dL/dq' ~ ldv : (T, Q, V) -> R
dL/dq  ~ ldq : (T, Q, V) -> R
d/dt   ~ dt  :  
w      ~ w   : T -> Q

\begin{code}
module DSLsofMath.Exercises.W03.E36 where
--import DSLsofMath.Algebra 

type T = Double  -- time
type Q = Double  -- coordinate
type V = Double  -- velocity
type R = Double  

l :: (T, Q, V) -> R
l (t, q, v) = undefined

ldq :: (T, Q, V) -> R
ldq (t, q, v) = m * v^2/2 - m * g * q
  where 
  g = 9.82
  m = undefined

const0 :: (T, Q, V) -> R
const0 _ = 0

w :: T -> Q 
w = undefined


expand :: (T -> Q) -> (T -> (T, Q, V))
expand w t = (t, w t, d w t)

path :: T -> R
path = l . expand w

lim :: R -> (R -> R) -> R
lim a f | a /= 0    = f a
        | otherwise = f 1e-5

psi :: (R -> R) -> (R -> R -> R)
psi f x h = (f (x + h) - f x) / h

d :: (R -> R) -> (R -> R)
d f = lim 0 . psi f 

d1 f (a, b, c) = d (f . fstNotFixed b c)   a
d2 f (a, b, c) = d (f . sndNotFixed a c)   b
d3 f (a, b, c) = d (f . thirdNotFixed a b) c

fstNotFixed :: a -> b -> (c -> (a, b, c))
fstNotFixed a b c = (a, b, c)

sndNotFixed :: b -> a -> (c -> (a, b, c))
sndNotFixed b a c =  (a, b, c)

thirdNotFixed :: c -> a -> (b -> (a, b, c))
thirdNotFixed c a b = (a, b, c)

\end{code}
