\documentclass{article}
%include polycode.fmt
\title{DSLs, sets and von Neumann}
\author{Group A1.10 Martin Fredin, Omar Bark} 
\begin{document}
\maketitle
%if False
\begin{code}
import Data.List (union, intersect, (\\), nub)
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Test.QuickCheck (quickCheck, (==>), Property, Gen, Arbitrary, arbitrary, sized, vectorOf)
import Data.Tuple.Extra (both)
\end{code}
%endif
Part 1:
\begin{code}
-- abstract syntax of set expressions with variables of type v
data TERM v = Empty 
            | Singleton    (TERM v)
            | Union        (TERM v) (TERM v)
            | Intersection (TERM v) (TERM v) 
            | Var          v 
            deriving Show
-- predicates over pure set expressions
data PRED v = Elem    (TERM v) (TERM v) 
            | Subset  (TERM v) (TERM v) 
            | And     (PRED v) (PRED v)
            | Or      (PRED v) (PRED v) 
            | Implies (PRED v) (PRED v) 
            | Not     (PRED v) 
            deriving Show
\end{code}
%if False
\begin{code}
newtype Set = S [Set]

instance Eq Set where
  (==) s s1 = null (xs \\ ys) && null (ys \\ xs) 
    where
    (xs, ys) = both (\(S xs) -> nub xs) (s, s1) 
  (/=) s s1 = not (s == s1)

instance Arbitrary Set where
  arbitrary = sized gen
    where 
    gen :: Int -> Gen Set
    gen size = if s == 0 then return (S []) else S <$> vectorOf size (gen (size `div` 5))
      where s = size `mod` 15
      
prop_equal :: Set -> Set -> Bool
prop_equal s@(S xs) s1@(S ys) =  S [s, s1]          == S [s1, s] 
                              && S [s, s]           == S [s] 
                              && S (xs ++ ys)       == S (ys ++ xs)
                              && S (xs ++ ys ++ xs) == S (ys ++ xs ++ xs)

instance Show Set where
  show = showVN
  
showPure (S [])     = "{}"
showPure (S (x:xs)) = "{" ++ show x ++ concatMap (\x' -> ", " ++ show x') xs ++ "}" 
-- will search forever if not an von Neumann number...
showVN x = show $ head [i | i <- [0 ..]
                          , eval ([] :: Env () Set) (vonNeumann i) == x]
prop_showVn :: Int -> Bool
prop_showVn i = let i' = i `mod` 15 in 
                show i' == showVN (eval ([] :: Env () Set) $ vonNeumann i')

type Env var dom = [(var, dom)]
\end{code}
%endif
Part 2:
\begin{code}
eval :: Eq v => Env v Set -> TERM v -> Set
eval env term = case term of
  Empty             -> S []         
  Singleton    t    -> S [eval env t]      
  Union        t t1 -> S $ f t `union` f t1
  Intersection t t1 -> S $ f t `intersect` f t1
  Var          v    -> fromJust $ lookup v env 
  where
  f = (\(S xs) -> xs) . eval env
  g Nothing  = error "variable is not in enviroment"
  g (Just s) = s
\end{code}
%if False
\begin{code}
-- testing
t, t1, t2, t3, t4  :: TERM Int
t  = Empty
t1 = Singleton Empty
t2 = Union t t1
t3 = Intersection t t2 
t4 = Var 2
env' :: Env Int Set
env' = [(0, S []), (1, S[S[]]), (2, S[S[], S[S[]]])] 
env      = [] :: Env () Set
set = S [S[], S[S[], S[S[]]], S[]]

\end{code}
%endif
\begin{code}
check :: Eq v => Env v Set -> PRED v -> Bool
check env pred = case pred of
  Elem    t t1 -> eval env t `elem` g t1
  Subset  t t1 -> all (`elem` g t1) $ g t
  And     p p1 -> f p && f p1  
  Or      p p1 -> f p || f p1
  Implies p p1 -> not (f p) || f p1
  Not     p    -> not $ f p
  where 
  f = check env 
  g = (\(S xs) -> xs) . eval env
\end{code}
%if False
\begin{code}
-- testing
p, p1, p2, p3, p4, p5, p6, p7, p8, p9 :: PRED Int
p  = Elem Empty $ Singleton Empty               -- True
p1 = Elem (Singleton Empty) $ Singleton Empty   -- False
p2 = Subset (Singleton Empty) $ Singleton Empty -- True
p3 = Subset (Var 0) (Var 2)                     -- True
p4 = Subset t1 t2                               -- True
p5 = And p p1                                   -- False
p6 = Or p p1                                    -- True
p7 = Implies p p1                               -- False
p8 = Implies p1 p                               -- True                
p9 = Not p                                      -- False

prop_PRED = f p && not (f p1) && f p2 && f p3 && f p4 && not (f p5)
          && f p6 && not (f p7) && not (f p9)
  where 
  f = check env'
\end{code}
%endif 
Part 3:
\begin{code}
vonNeumann :: Int -> TERM v
vonNeumann 0 = Empty
vonNeumann n = Union (vonNeumann $ n - 1) (Singleton (vonNeumann $ n - 1))

claim :: Int -> Int -> Bool
claim i i1 | (#) n <= (#) n = check env (Subset n n1)
  where 
  (n, n1)  = (vonNeumann i, vonNeumann i1)
  (#)      = (\(S xs) -> length xs) . eval env

claim1 :: Int -> Bool 
claim1 i  = n i == S [n j | j <- [0 .. i - 1]]
  where
  n = eval env . vonNeumann 
\end{code}
\end{document}
