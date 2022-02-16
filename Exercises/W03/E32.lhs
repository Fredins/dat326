\begin{code}
import qualified Prelude 
import Prelude (Integer, (<), (>), (==), undefined, (-), ($), Bool, otherwise, Int, Rational)

class Additive a where
  zero :: a
  (+)  :: a -> a -> a
class Additive a => AddGroup a where
  negate :: a -> a
class Multiplicative a where
  one :: a
  (*) :: a -> a -> a
class Multiplicative a => MulGroup a where
  recip :: a -> a 

(^) :: MulGroup a => a -> Int -> a
(^) x i | i == 0    = one
        | i > 0     = f i (*) x
        | otherwise = f (Prelude.negate i) (\x acc -> recip x * acc) x 
  where 
  f :: (MulGroup a) => Int -> (a -> a -> a) -> a -> a
  f i g acc = if i == 1 then acc else f (i - 1) g acc

\end{code}
