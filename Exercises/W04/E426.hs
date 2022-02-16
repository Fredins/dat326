import Prelude (Double, lookup, Eq, Show, (==), Integer, Double, ($), String)
import Data.Maybe (fromJust)
import DSLsofMath.Algebra hiding (Ring)


-- i
 {-
class Ring a where
  zero   :: a
  (+)    :: a -> a -> a 
  negate :: a -> a
  one    :: a
  (*)    :: a -> a -> a 
-}
class (AddGroup a, Multiplicative a) => Ring a where

-- ii
{-
instance Ring (RingExp v) where
  zero   = Con 0
  (+)    = Add
  negate = Neg 
  one    = Con 1
  (*)    = Mul
-}
data RingExp v = Con Integer
               | Add (RingExp v) (RingExp v)
               | Mul (RingExp v) (RingExp v)
               | Neg (RingExp v) 
               | Var v deriving (Eq, Show)

instance Additive (RingExp v) where zero = Con 0; (+) = Add
instance AddGroup (RingExp v) where negate = Neg 
instance Multiplicative (RingExp v) where one = Con 1; (*) = Mul
instance Ring (RingExp v) where


-- iii Integer and Double
instance Ring Integer where
instance Ring Double where

-- iv
type Env a b = [(a, b)]

eval :: (Ring b, Eq a) => Env a b -> RingExp a -> b
eval env = ev
  where 
  ev (Add r r1) = ev r + ev r1
  ev (Mul r r1) = ev r * ev r1
  ev (Neg r)    = negate $ ev r
  ev (Var a)    = fromJust (lookup a env) 
  ev (Con a)    = fromInteger a

-- v
t  = Add (Con 4) (Con 3)
t1 = Mul (Con 3) (Neg (Con 2))
t2 = Mul (Var "x") (Var "y")

--env   = [("x", 2), ("y", 5)]
evalInteger :: Eq a => Env a Integer -> RingExp a -> Integer
evalInteger env = eval env

envI = [("x", 2), ("y", 5)] :: Env String Integer
testI  = evalInteger envI t
testI1 = evalInteger envI t1
testI2 = evalInteger envI t2

evalDouble :: (Eq a) => Env a Double -> RingExp a -> Double
evalDouble env = eval env
envD = [("x", 2.5), ("y", 5)] :: Env String Double
testD  = evalDouble envD t
testD1 = evalDouble envD t1
testD2 = evalDouble envD t2


