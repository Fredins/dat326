
h0 (h, op1, op2) = forall x. h op1 == op2 
h1 (h, op1, op2) = forall x. h (op1 x) == op2 (h x)
h2 (h, op1, op2) = forall x. forall y. h (op1 x y) == op2 (h x) (h y)

h0 (f, zero, zero)
f zero == zero

h1 (f, negate, negate)
f (negate x) == negate (f x)

h2 (f, (+), (+))
f (one + x) == f one + f x 
f x == one + f (x - one)



