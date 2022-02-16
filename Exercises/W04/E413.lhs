\begin{code}
import Prelude (pi)
import DSLsofMath.FunExp
import DSLsofMath.Algebra


instance Additive FunExp where
  zero = Const 0
  (+)  = (:+:)

instance AddGroup FunExp where 
  negate = Negate

instance Multiplicative FunExp where
  one = Const 1
  (*) = (:*:)

instance MulGroup FunExp where
  recip = Recip

instance Transcendental FunExp where
  pi  = Const Prelude.pi
  exp = Exp
  sin = Sin
  cos = Cos

\end{code}
