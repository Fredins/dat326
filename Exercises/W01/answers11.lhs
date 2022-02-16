\begin{code}
import Numeric.Ring.Class  

newtype  ComplexSem r  =  CS  (r , r) deriving Eq
data ComplexSyn r  =  ToComplexCart r r
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r

embed :: ComplexSem r -> ComplexSyn r
embed (CS (x,y)) = ToComplexCart x y

eval :: Ring r => ComplexSyn r -> ComplexSem r
eval = undefined

\end{code}

step 0: e :: ComplexSyn r

step 1: semantic equality is desirable, because
multiple expression with different can have the 
same sematic, f.ex. 
 - ToComplexCart 1 0
 - ToComplexCart 2 0 :+: ToComlexCart (-1) 0 
Thereby, the equality property should satisfy
(===) c c1 = eval c == eval c1

step 2: The round-trip property concurrs with sematic equality, 
because it doesn't matter how to sematic s is embed. The eval
function will return the same sematic for symatically equal 
ComplexSyn (syntax).


