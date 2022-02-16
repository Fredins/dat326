\begin{code}
import Test.QuickCheck (quickCheck)

isoR         :: (Bool -> t) -> (t, t)
isoR f       =  (f True, f False)
isoL         :: (t, t) -> (Bool -> t)
isoL (t, t1) = f
  where 
  f True  = t
  f False = t1

prop_iso :: Eq a => (a,a) -> Bool
prop_iso as =  f True  == (isoL . isoR) f True
            && f False == (isoL . isoR) f False
            && as  == (isoR . isoL) as
  where 
  f True  = "Hello"
  f False = "World"

\end{code}
