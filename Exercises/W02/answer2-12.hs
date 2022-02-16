import Test.QuickCheck(quickCheck, Gen, Arbitrary, arbitrary, elements)
import Debug.Trace (trace)

type VarT = String 
data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT 
  deriving (Show, Eq)

instance Arbitrary RatT where
  arbitrary = FromI <$> arbitrary 

type PSym = String
data FOL  =  Implies FOL FOL | And FOL FOL | Or FOL FOL | Not FOL
          |  FORALL  VarT  FOL    |  EXISTS  VarT  FOL
          |  PName PSym [RatT]    |  Equal  RatT  RatT
  deriving (Show, Eq)

instance Arbitrary FOL where
  arbitrary = do
    r  <- arbitrary 
    r1 <- arbitrary  
    s  <- arbitrary 
    s1 <- arbitrary
    c  <- elements [Implies, And, Or]
    return (c (PName s r) (PName s1 r1))

morgan (Not (a `Or` b))    = Not a `And` Not b
morgan (Not a `And` Not b) = Not (a `Or` b)
morgan (Not (a `And` b))   = Not a `Or` Not b
morgan (Not a `Or` Not b)  = Not (a `And` b)
morgan _                   = error "morgan" 

p, p1, p2, p3 :: FOL -> FOL ->  FOL
p  a b = Not (a `Or` b) 
p1 a b = Not a `And` Not b
p2 a b = Not (a `And` b)
p3 a b = Not a `Or` Not b

prop_morgan :: FOL -> FOL -> Bool
prop_morgan a b =  p  a b == morgan (morgan (p a b))
                && p1 a b == morgan (p a b)
                && p  a b == morgan (p1 a b)
                && p2 a b == morgan (p3 a b)
                && p3 a b == morgan (p2 a b)
                  


