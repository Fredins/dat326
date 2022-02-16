
\begin{code}

newtype Addition = Addition Double

zero :: Addition 
zero = Addition 0

instance Semigroup Addition where
  (<>) (Addition x) (Addition y) = Addition (x + y)

instance Monoid Addition where
  mempty = zero
  
\end{code}

