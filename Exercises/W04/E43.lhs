
H2 (h, op1, op2) = forall x. forall y. h (op1 x y) == op2 (h x) (h y)

1. 
evalE : ComplexE -> ComplexD
Plus -> plusD

\begin{code}
import DSLsofMath.FunExp
import DSLsofMath.Algebra (Transcendental)

h2 :: (Transcendental b, Eq b, Num b) => ((a -> b -> b), (a -> a -> a), (b -> b -> b)) -> a -> a -> Bool
h2 (h, op1, op2) x y = h (op1 x y) == op2 (h x) (h y)

test = h2 (eval :: FunExp -> Double -> Double, (:*:), (*)) (Const 2) (Const 3)


\end{code}
