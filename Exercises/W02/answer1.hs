import Data.List (nub)

-- 2.1
data Prop  =  Implies  Prop  Prop  |  And      Prop  Prop  |  Or       Prop  Prop
           |  Not      Prop        |  Name     Name        |  Con      Bool   deriving Eq
type Name = String
type Env = Name -> Bool

isTautology :: Prop -> Bool
isTautology p = and (map (eval p) (envs (freeNames p)))

eval :: Prop -> Env -> Bool
eval (Implies p q)  env = eval p env  ==>  eval q env
eval (And p q)      env = eval p env  &&   eval q env
eval (Or  p q)      env = eval p env  ||   eval q env
eval (Not p)        env = not (eval p env)
eval (Name n)       env = env n
eval (Con t)        env = t

(==>) :: Bool -> Bool -> Bool
False  ==> _              = True
True   ==> p              = p

envs :: [Name] -> [Env]
envs []      =  [error "envs: never used"]
envs (n:ns)  =  [  \n' -> if n == n' then b else e n'
                |  b  <-  [False, True]
                ,  e  <-  envs ns
                ]

freeNames :: Prop -> [Name]
freeNames(Implies p q) = nub $ freeNames p ++ freeNames q
freeNames (And p q)    = nub $ freeNames p ++ freeNames q
freeNames (Or  p q)    = nub $ freeNames p ++ freeNames q 
freeNames (Not p)      = nub $ freeNames p   
freeNames (Name n)     = [n]  
freeNames (Con t)      = []

p1, p2, p3, p4 :: Prop
p1 = And  (Name "a")  (Not (Name "a"))
p2 = Or   (Name "a")  (Not (Name "a"))
p3 = Implies  (Name "a")  (Name "b")
p4 = Implies  (And a b)   (And b a)
  where a = Name "a"; b = Name "b"
-- 2.2
checkProof TruthIntro        (Con True)   =   True
checkProof (AndIntro t u)    (And p q)    =   checkProof t p
                                          &&  checkProof u q
checkProof (OrIntroL t)      (Or p q)     =   checkProof t p
checkProof (OrIntroR u)      (Or p q)     =   checkProof u q
checkProof (NotIntro q t u)  (Not p)      =   checkProof t (p `Implies` q)
                                          &&  checkProof u (p `Implies` Not q)
checkProof (AndElimL q t)       p  =   checkProof  t  (p `And` q)
checkProof (AndElimR p t)       q  =   checkProof  t  (p `And` q)
checkProof (OrElim p q t u v)   r  =   checkProof  t  (p `Implies` r)
                                   &&  checkProof  u  (q `Implies` r)
                                   &&  checkProof  v  (Or p q)
checkProof (NotElim t)          p  =   checkProof  t  (Not (Not p))
checkProof (FalseElim t)        p  =   checkProof  t  (Con False)
checkProof (Assume p')          p                =   p == p'
checkProof (ImplyIntro f)       (p `Implies` q)  =   checkProof (f (Assume p)) q
checkProof (ImplyElim p t u)    q                =   checkProof t (p `Implies` q)
                                                 &&  checkProof u p
checkProof _                    _                =   False -- incorrect proof


data Proof  =  TruthIntro                   |  FalseElim Proof
            |  AndIntro  Proof  Proof
            |  AndElimL  Prop   Proof       |  AndElimR  Prop  Proof
            |  OrIntroL  Proof              |  OrIntroR  Proof
            |  OrElim Prop Prop Proof Proof Proof
            |  NotIntro Prop Proof Proof    |  NotElim Proof
            |  Assume Prop
            |  ImplyIntro (Proof -> Proof)  |  ImplyElim  Prop  Proof Proof



conjunctionComm :: Prop
conjunctionComm = p4

conjunctionCommProof :: Proof
conjunctionCommProof = ImplyIntro step
  where  step :: Proof -> Proof
         step evAB =  AndIntro  (AndElimR  (Name "a")  evAB  )
                                (AndElimL  (Name "b")  evAB  )
testCommProof = checkProof conjunctionCommProof conjunctionComm
testCommProof' = checkProof conjunctionCommProof' conjunctionComm

conjunctionCommProof' :: Proof
conjunctionCommProof' = ImplyIntro step
  where  step :: Proof -> Proof
         step evAB =  AndIntro  (AndElimL  (Name "b")  evAB  )
                                (AndElimR  (Name "a")  evAB  )



