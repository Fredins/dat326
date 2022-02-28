{-# LANGUAGE  FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RebindableSyntax #-}
import           DSLsofMath.Algebra
import           DSLsofMath.Derive
import           DSLsofMath.FunExp       hiding ( derive )
import           DSLsofMath.FunExpInst
import           DSLsofMath.FunNumInst
import           DSLsofMath.Simplify            ( simplify )
import           Data.List.Extra                ( replace )
import           Data.Ratio
import           Data.Tuple.Extra               ( both )
import           Debug.Trace                    ( trace )
import           GHC.Real                       ( infinity )
import           Prelude                 hiding ( (*)
                                                , (+)
                                                , (-)
                                                , (/)
                                                , (^)
                                                , cos
                                                , exp
                                                , fromInteger
                                                , fromRational
                                                , isNaN
                                                , negate
                                                , pi
                                                , recip
                                                , sin
                                                )
import qualified Prelude
import           Test.QuickCheck                ( Arbitrary
                                                , Gen
                                                , arbitrary
                                                , choose
                                                , elements
                                                , frequency
                                                , getSize
                                                , quickCheck
                                                , sample
                                                , sized
                                                , suchThat
                                                , vectorOf
                                                )
import           Text.Printf


-- Part 1 a
{-
eval'' :: FunExp -> FunSem
eval'' = eval . derive . derive

For eval'' to be a homomorphism it needs to accurately 
map FunExp -> FunSem for all the constructors of FunExp.


h(eval'', Const, const)               obs: the third argument is a
h(eval'', X, id)                           function from Prelude 
h(eval'', :+:, +)                           
h(eval'', :*:, *)
h(eval'', Recip, recip)   
h(eval'', Negate, negate)
h(eval'', Exp, exp)
h(eval'', Sin, sin)
h(eval'', Cos, cos)

Thereby, if any of these aren't true then eval'' is not 
a homomorphism from FunExp to FunSem. Keep in mind that
e.g. h(eval'', Const, const) should be true for all inputs. 
So forall x:. eval'' (Const x) == const (eval'' x).

h(eval'', :*:, *) for e.g. x = (X :*: Const 1), y = X. 
Proof: 
LHS = eval'' ((:*:) x y)
RHS = (*) (eval'' x) (eval'' y)

LHS                            = 
eval'' ((X :+: Const 1) :*: X) = { Perfom double derivation and eval,  
                                 (x + 1) * x -> x^2 + x -> 2x + 1 -> 2 }
2                                

HL                                   =
eval'' (X :+: Const 1)  * eval'' X   = { perform both of the double derivations, and eval,  
                                       x + 1 -> 1 -> 0, X -> 1 -> 0  }  
0 :*: 0                              = { multiplication property a*0 = 0}
0                              

LHS /= RHS !

-}



-- Part 1 b


type R = Double
type T a = (a, a, a)   -- (a, a', a'') position, speed, acceleration
type TF a = T (a -> a) -- = |(a->a, a->a, a->a)|
type FT a = a -> T a   -- = |a -> (a, a, a)|

-- like 'both' but for functions with different arity
three0 f = (f, f, f)
three1 f (x, y, z) = (f x, f y, f z)
three2 f (x, y, z) (x1, y1, z1) = (f x x1, f y y1, f z z1)

instance Additive a => Additive (T a) where
  (+)  = addT
  zero = zeroT
instance (Additive a, Multiplicative a) => Multiplicative (T a) where
  (*) = mulT
  one = oneT
instance AddGroup a => AddGroup (T a) where
  negate = negateT
instance (AddGroup a, MulGroup a) => MulGroup (T a) where
  recip = recipT
instance Transcendental a  => Transcendental (T a) where
  pi  = piT
  sin = sinT
  cos = cosT
  exp = expT

addT :: Additive a => T a -> T a -> T a
addT = three2 (+)
zeroT :: Additive a => T a
zeroT = three0 zero
mulT :: (Additive a, Multiplicative a) => T a -> T a -> T a
mulT (x, x', x'') (y, y', y'') = (x * y, z', z'')
 where
  z'  = x' * y + x * y'
  z'' = (x'' * y + x' * y') + (x' * y' + x * y'')
oneT :: (Additive a, Multiplicative a) => T a
oneT = (one, zero, zero)
negateT :: AddGroup a => T a -> T a
negateT = three1 negate
recipT :: Field a => T a -> T a
recipT (x, x', x'') = (recip x, y', y'')
 where
  y'  = negate x' * recip x ^ 2
  y'' = negate (x'' * x - two * x' ^ 2) * recip x ^ 3
piT :: Transcendental a => T a
piT = (pi, zero, zero)
sinT :: Transcendental a => T a -> T a
sinT (x, x', x'') = (sin x, y', y'')
 where
  y'  = cos x * x'
  y'' = negate sin x * x' ^ 2 + cos x * x''
cosT :: Transcendental a => T a -> T a
cosT (x, x', x'') = (cos x, y', y'')
 where
  y'  = negate sin x * x'
  y'' = negate (cos x * x' ^ 2 + sin x * x'')
expT :: Transcendental a => T a -> T a
expT (x, x', x'') = (exp x, y', y'')
 where
  y'  = exp x * x'
  y'' = exp x * (x' ^ 2 + x'')





-- Part 1 c
evalDD :: Transcendental a => FunExp -> a -> T a
evalDD (Const alpha) = const (fromRational $ toRational alpha, zero, zero)
evalDD X             = (, one, zero)
evalDD (e1 :+: e2)   = evalDD e1 + evalDD e2
evalDD (e1 :*: e2)   = evalDD e1 * evalDD e2
evalDD (Recip  e )   = recip (evalDD e)
evalDD (Negate e )   = negate (evalDD e)
evalDD (Exp    e )   = exp (evalDD e)
evalDD (Sin    e )   = sin (evalDD e)
evalDD (Cos    e )   = cos (evalDD e)


















-- testing

-- manage floating point equality
infix 4 ~=
(~=) :: (AddGroup a, Transcendental a, Ord a) => T a -> T a -> Bool
(~=) a b = a - b < exp (-5)

prop_inst :: (Multiplicative a, Transcendental a, Ord a) => T a -> Bool
prop_inst a = sin a ^ 2 + cos a ^ 2 ~= one

instance Arbitrary FunExp where
  arbitrary = getSize `suchThat` (> 0) >>= gen

gen 1 = elements [const X, Const] >>= \x -> x . fromInteger <$> arbitrary -- decimals ugly
gen 2 = frequency [(1, recipOp 2), (4, unary 2)]
gen i = frequency [(1, recipOp i), (4, unary i), (2, binary i)]
unary, recipOp, binary :: Int -> Gen FunExp
unary i = elements [Negate, Exp, Cos, Sin] >>= (<$> gen (i - 1))
recipOp i = Recip <$> (gen (i - 1) `suchThat` (\e -> eval e (0 :: REAL) /= 0))
binary i = elements [(:+:), (:*:)]  >>= \op -> sequence [a, b] >>= \[x, y] -> return (op x y)
  where (a, b) = both gen $ splitInt (i - 1)
splitInt :: Int -> (Int, Int)
splitInt i | even i    = (half, half)
           | otherwise = (half + 1, half)
  where half = i `div` 2

testGenFunExp = sample (arbitrary :: Gen FunExp)

prop_evalDD :: (Transcendental a, Eq a, Ord a) => FunExp -> a -> Bool
prop_evalDD a x = assertReal
  (evalDD a x)
  (eval a x, (eval . derive) a x, (eval . derive . derive) a x)

assertReal :: (Transcendental a, Eq a, Ord a) => T a -> T a -> Bool
assertReal a b | anyP isInf a || anyP isNaN a = True
               | otherwise                    = a ~= b
anyP p (x, x', x'') = p x || p x' || p x''
isInf, isNaN :: (Transcendental a, Eq a, Ord a) => a -> Bool
isInf x | x < zero  = negate x == fromRational infinity
        | otherwise = x == fromRational infinity
isNaN x = x /= x



















-- part 1 d
{-

h2 (evalDD, (:*:), (*)) x y =  == (*) (evalDD x) (evalDD y)

LHS                       = 
evalDD ((:*:) x y)        = { infix op.  } 
evalDD (x :*: y)          = { def. evalDD for :*: }
evalDD x * evalDD y       = { infix op.  } 
(*) (evalDD x) (evalDD y) = 
RHS

-}

-- part 2

newtonT :: (T R -> T R) -> R -> R -> R
newtonT f e = head . newtonTList f e

newtonTList :: (T R -> T R) -> R -> R -> [R]
newtonTList tupf e y = n y []
 where
  n :: R -> [R] -> [R]
  n x acc | abs fx < e  = acc'
          | f'x /= zero = n x1 acc'
          | otherwise   = n (x + e) acc'

   where
    fx, f'x :: R
   -- (fx, f'x, _) = tupf (x, x, x)
    (fx, f'x, _) = tupf (x, one, zero)
-- (x^3, 3x^2 * x')

    x1           = x - (fx / f'x)
    acc'         = x : acc



-- part 3

-- "getters"
g1, g2, g3 :: (a, a, a) -> a
g1 (x, _, _) = x
g2 (_, x, _) = x
g3 (_, _, x) = x

-- apply
apply :: T a -> TF a -> T a
apply (x, y, z) (f, g, h) = (f x, g y, h z)





optim :: (T R -> T R) -> R -> R -> String
optim tupf e x | abs f''x0 < 0.1   = "Dunno x = " ++ y
               | signum f''x0 == 1 = "Minimum at x = " ++ y
               | otherwise         = "Maximum at x = " ++ y

 where
  x0 = newtonT tupf' e x
  tupf' :: T R -> T R
  tupf' a = (g2 $ tupf a, g3 $ tupf a, zero)
  f''x0 = (g3 . tupf) (x0, one, zero)
  y     = printf "%0.2f" x0
















-- testing 
evalT :: T FunExp -> R -> T R
evalT (x, x', x'') y = (eval x y, eval x' y, eval x'' y)

-- just for wrapping my head around the difference between 
-- T FunExp and TF FunExp
testTupleFuncFunExp = putStrLn str
 where
  str = "g = " ++ show g ++ "\ng' = " ++ show g' ++ "\ng'' = " ++ show g''
  g, g', g'' :: FunExp
  (g, g', g'') = three1 simplify $ f t
  f :: T FunExp -> T FunExp
  f = (^+ 2)
  t = (X, Const 1, Const 0)

test0 :: Ring a => a -> a
test0 = (^+ 2)                      -- roots x=0 (double)
test1 :: Ring a => a -> a
test1 x = x ^+ 2 - one                -- roots x=-1 x=1
test2 :: Transcendental a => a -> a
test2 = sin                        -- roots x=pi*n, n:Z
test3 n x y = y ^ n - three0 x        -- roots depends on n
test4 :: Ring a => a -> a
test4 = (^+ 3)                       -- roots x = 0
test5 :: Field a => a -> a
test5 x = -x ^ 2                      -- roots x = 0 (double)

prop_optim :: R -> Bool
prop_optim x =
  check test0 x
    == "Minimum"  -- x^2
    && check test1 x
    == "Minimum"  -- x^2 - 1
    && check test5 x
    == "Maximum"  -- -x^2 
    && check test4 x
    == "Dunno"    -- x^3
  where check f y = (head . words) $ optim f 1e-9 y

