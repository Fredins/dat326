data Exp  =  Con Integer
          |  Plus   Exp  Exp
          |  Minus  Exp  Exp
          |  Times  Exp  Exp
          |  Var    String
  deriving (Eq, Show)

-- 1.1
a1 = Plus (Con 2) $ Con 2
a2 = Plus a1 $ Con (7 * 9)
a3 = Minus (Con (8 * (2 + 11))) (Times (Con (3 + 7)) (Plus a1 a2))

eval :: Exp -> Integer

eval e = case e of
  (Con x      ) -> x
  (Plus  e1 e2) -> eval e1 + eval e2
  (Minus e1 e2) -> eval e1 - eval e2
  (Times e1 e2) -> eval e1 * eval e2
  (Var s      ) -> varVal s

c1 =
  Times (Times (Minus (Var "x") $ Con 15) (Plus (Var "y") $ Con 12)) $ Var "z"

varVal :: String -> Integer
varVal s | s == "x"  = 5
         | s == "y"  = 8
         | s == "z"  = 13
         | otherwise = error "varVal"


