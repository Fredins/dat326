h0 (exp, zero, one)
h1 (exp, (+), (*))
h1 (exp, negate, recip)

exp zero       == one
exp (x + y)    == exp x * exp y
exp (negate n) == recip (exp n) 

log one        == zero
log (x * y)    == log x + log y
log (recip n)  == log one + negate (log n)

Addgroup
- zero
- (+)
- negate

MulGroup
- one
- (*)
- recip


