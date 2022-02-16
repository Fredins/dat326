
Ring = commutative, additive, monoid , multiplication distrubutes over addition.

\begin{code}
import DSLsofMath.Algebra (AddGroup, Multiplicative, negate, Ring, zero, Additive)
import qualified DSLsofMath.Algebra as A ((+))

newtype Addition = Addition Double


instance Semigroup Addition where
  (<>) (Addition x) (Addition y) = Addition (x + y)

instance Monoid Addition where
  mempty = Addition 0

instance Additive Addition where
  (+)  = (<>)
  zero = mempty
 
instance AddGroup Addition where
  negate (Addition x) = Addition (-x)

  
\end{code}

