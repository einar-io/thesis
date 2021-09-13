open import "lmaplib"
--Example program 2:
--arg: (Scalar 10.0)
--fun: (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dupe)
--res: (Pair (Scalar 30.0, Scalar 70.0))

let fun1 = (sv 3.0)
let fun2 = (sv 7.0)
let fun3 = (para fun1 fun2)
let fun4 = (dupe)
let fun5 = (comp fun3 fun4)

entry main = fun5 -- ([30.0f32], [70.0f32])
