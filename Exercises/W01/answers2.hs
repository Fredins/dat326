-- 1.2
import Data.Maybe (fromJust)

data E2 a  =  Con a
           |  Var String
           |  Plus   (E2 a)  (E2 a)
           |  Minus  (E2 a)  (E2 a)
           |  Times  (E2 a)  (E2 a)
  deriving(Eq, Show)

a1 = \a -> Plus (Con 2.0) a
a2 = \b c -> Plus (Con 5.3) $ Times b c
a3 = \a b c d e f -> Minus (Times a $ Plus b c) (Times (Plus d e) $ Plus f a)

type Table a = [(String, a)]

vars :: Table Double
vars = [("a", 1.5), ("b", 4.8), ("c", 2.4), ("d", 7.4), ("e", 5.8), ("f", 1.7)]

varVal :: Table a -> String -> a
varVal t s = case lookup s t of
  (Just x) -> x
  _        -> error "varVal"

eval :: Num a => Table a -> E2 a -> a
eval t e = case e of 
  (Con x)       -> x
  (Var s)       -> fromJust $ lookup s t
  (Plus e1 e2)  -> ev e1 + ev e2
  (Minus e1 e2) -> ev e1 - ev e2
  (Times e1 e2) -> ev e1 * ev e2
  where ev = eval t

a4 = eval vars (Plus (Con 2) (Var "a")) == 3.5 
