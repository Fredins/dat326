exercise 3.3

fromRational :: (AddGroup a, MulGroup a) => Rational -> a
fromRational i = undefined
  where 
   y = (whole * den + one) * recip den 
   (whole, den) = g (f i (0, 0))
   g (w, r) =  (fromInteger w, fromInteger (Prelude.recip r))
   f :: Rational -> (Integer, Rational) -> (Integer, Rational)
   f i (w,r) | i < 0     = (w, i) 
             | otherwise = f (i - 1) (w Prelude.+ 1, i)






