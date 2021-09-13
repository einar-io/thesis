open import "lmaplib"
--Example program:
--ARG: (Tensor [Scalar 1.0, Scalar 2.0])
--fun: (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
--RES: (Tensor [Scalar 168.0, Scalar 336.0])

let fun1 = (sv 3.0)
let fun2 = (sv 7.0)
let fun3 = (comp fun1 fun2)
let fun4 = (sv 8.0)
let fun5 = (comp fun3 fun4)

entry main = fun5	
