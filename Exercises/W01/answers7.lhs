\begin{code}

s2p :: (Either b c -> a) -> (b -> a, c -> a)
s2p f = (f . Left, f . Right)
  
p2s :: (b -> a, c -> a) -> (Either b c -> a)
p2s (f, g) = h
  where
  h (Left b)  = f b
  h (Right c) = g c

\end{code}

