const :: a -> b -> a


h2 (f, op1, op2) = forall x. forall y. f (op1 x y) == op2 (f x) (f y)

h2 (const, (+), (+))


const (a + (b -> a))

