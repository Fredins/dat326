\documentclass{article}
%include polycode.fmt
\begin{document}
\begin{code}
import Data.Ratio

type VarT = String
data RatT = RV VarT | FromI Integer | RPlus RatT RatT | RDiv RatT RatT
  deriving Show

type RatSem = Rational
evalRat :: RatT -> (VarT -> RatSem) -> RatSem
evalRat (RV s)       f = f s 
evalRat (FromI i)    f = fromInteger i
evalRat (RPlus r r1) f = evalRat r f + evalRat r1 f
evalRat (RDiv r r1)  f = evalRat r f / evalRat r1 f

testEval = evalRat r f == 4
  where
  f "x" = 2 
  f "y" = 3
  f  _  = error "missing"
  r = RDiv (RPlus (RV "x") $ FromI 10) $ RV "y"

-- 2.6
type PSym = String
data FOL  =  Implies FOL FOL | And FOL FOL | Or FOL FOL | Not FOL
          |  FORALL  VarT  FOL    |  EXISTS  VarT  FOL
          |  PName PSym [RatT]    |  Equal  RatT  RatT
  deriving Show

Not (a  `Or`    b)  =  Not a  `And`  Not b
Not (a'  `And`  b') =  Not a' `Or`   Not b'

morgan (Not (a `Or` b))    = Not a `And` Not b
morgan (Not a `And` Not b) = Not (a `Or` b)
morgan (Not (a `And` b))   = Not a `Or` Not b
morgan (Not a `Or` Not b)  = Not (a `And` b)
morgan _                   = error "morgan" 
\end{code}



\end{document}
