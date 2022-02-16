exp :: R -> R>0
log :: R>0 -> R

-- exp
h0 (exp, zero, one) = exp zero == one { a^0 = 1 }
h1 (exp, (+), (*)) = exp (x + y) == exp x * exp y {a^(x+y) = a^x * a^y }

-- log
h0 (log, 1, 0) = log 1 == 0
h1 (log, (*), (+)) = log(ab) = log a + log b

exp map from + to * while log do the inverse. It also follows that exp . log = id.








