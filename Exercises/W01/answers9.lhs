\begin{code}
(1+) = \x -> 1 + x
(*2) = \x -> x * 2
((1+) . (*2)) = \x -> 1 + (x * 2)
((*2) . (1+)) = \x -> (1 + x) * 2
((+1) . (^2)) = \x -> x^2 + 1
((^2) . (+1)) = \x -> (x + 1)^2
((a+) . (b+)) = \x -> a + b + x
\end{code}