polar form: w = r*cosø + i*r*sinø

\begin{code}
import Test.QuickCheck (quickCheck)

newtype ComplexSem r = CS (r, r) deriving (Eq, Show)
data ComplexSyn r = ToComplexCart r r
                  | ComplexSyn r :+: ComplexSyn r
                  | ComplexSyn r :*: ComplexSyn r deriving (Show)

eval :: (Floating r, Fractional r, Num r) => ComplexSyn r -> ComplexSem r
eval (ToComplexCart r a) = CS (r, a)
eval (c :*: c')          = CS (r * r', a + a')
  where 
  (CS (r, a))            = eval c
  (CS (r', a'))          = eval c'
eval (c :+: c')          = toPolar $ CS (a + a', b + b')
  where 
  (CS (a, b))   = toCart $ eval c
  (CS (a', b')) = toCart $ eval c'
  

toCart :: (Floating r, Fractional r, Num r) => ComplexSem r -> ComplexSem r 
toCart (CS (r, a)) = CS (r * cos a, r * sin a)

toPolar :: (Floating r, Fractional r, Num r) => ComplexSem r -> ComplexSem r
toPolar (CS (a, b)) = CS (sqrt (a^2 + b^2), atan (b / a))

embed :: Num r => ComplexSem r -> ComplexSyn r
embed (CS (x,y)) = ToComplexCart x y

syn = ToComplexCart 2 $ 0.5 * pi 
syn1 = ToComplexCart 1 (0.3 * pi) :*: ToComplexCart 2 (0.2 * pi)

prop_equal = syn == syn1 


instance (Floating r, Fractional r, Num r, Eq r) => Eq (ComplexSyn r) where
 (==) c c1 = eval c == eval c1



\end{code}
