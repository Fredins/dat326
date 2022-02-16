exercise 3.5

\begin{code}

type REAL = Double
data FunExp  =  Const REAL
             |  X
             |  FunExp  :+:  FunExp
             |  FunExp  :*:  FunExp
             |  Exp FunExp

derive :: FunExp -> FunExp
derive (Const a)   = Const 0
derive X           = Const 1
derive (e1 :+: e2) = derive e1 :+: derive e2
derive (e1 :*: e2) = (derive e1 :*: e2) :+: (e1 :*: derive e2)
derive (Exp e)     = Exp e :*: derive e

\end{code}
