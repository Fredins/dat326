\begin{code}
import Numeric.Natural
import Data.Ratio

type Nat    =  Natural     -- imported from |Numeric.Natural|
type QQP    =  Ratio Nat   -- imported from |Data.Ratio|
type Seq a  =  Nat -> a

liftSeq1 :: (a->b) -> Seq a -> Seq b
liftSeq1 h f i = h (f i)              -- |{h (f 0), h (f 1), h (f 2), ...}|

liftSeq0 :: a -> Seq a
liftSeq0 c i = c

conSeq :: a -> Seq a
conSeq c i = c             -- |{c, c, c, c, ...}|
\end{code}

(1+) . a    --  {1 + a0, 1 + a1, 1 + a2 ..}
a . (1+)    --  {a1, a2, a3 ..}

liftSeq0 and conSeq are the same function.
liftSeq1 is a more well typed form of fmap.
