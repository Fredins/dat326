\begin{code}
import Test.QuickCheck (quickCheck)

f2p :: (a -> (b,c)) -> (a -> b, a -> c)
f2p f = (fst . f, snd . f) 

p2f :: (a -> b, a -> c) -> (a -> (b, c))
p2f (f, g) a = (f a, g a)

f x = (x, 2*x)
g x = x
h x = 2*x

prop_f2p x = (g' x, h' x) == (g x, h x)
  where
  (g', h') = f2p f

prop_p2f x = p2f (g, h) x == f x

\end{code}
