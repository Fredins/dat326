data ComplexSyn r  =  ToComplexCart r r
                   |  Var String
                   |  ComplexSyn r  :+:  ComplexSyn r
                   |  ComplexSyn r  :*:  ComplexSyn r deriving (Eq, Show)

instance (Num a, Eq a) => Num (ComplexSyn a) where
  (+) = (:+:)
  (*) = (:*:)
  abs = \(ToComplexCart r r1) -> ToComplexCart (abs r) $ abs r1
  signum = \c -> if abs c == c then 1 else -1
  negate (ToComplexCart r r1) = ToComplexCart (negate r) (negate r)
  negate (c :+: c1) = negate c :+: c1 
  negate (c :*: c1) = negate c :*: negate c1 
  negate a = ToComplexCart (-1) 0 :*: a
  fromInteger =  \i -> ToComplexCart (fromInteger i) 0 

test = Var "z" * ToComplexCart 2.0 0
test1 :: ComplexSyn Double
test1 = 3


