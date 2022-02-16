
apply x f = f x
apply :: A -> (A -> B) -> B
apply c :: (A -> B) -> B

additive 
 - h0 (f, zero, zero)
 - h2 (f, (+), (+)) 

-- h0 apply c zero == zero
apply c zero = { def. apply } 
zero c       = { def. zero for functions }
zero           

-- h2 apply c (f + g) == apply c f + apply c g
apply c (f + g)       = { def. + for functions }
apply c f + apply c g =



