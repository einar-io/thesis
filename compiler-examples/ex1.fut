open import "lmaplib"
-------- PROCEDURE
-- 

--Example program:
--ARG: (Tensor [Scalar 1.0, Scalar 2.0])
--FUN: (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
--RES: (Tensor [Scalar 168.0, Scalar 336.0])

-- print input to program
let ARG : []f32 = [1.0,2.0]

let FUN1 = (sv 3.0)
let FUN2 = (sv 7.0)
let FUN3 = (comp FUN1 FUN2)
let FUN4 = (sv 8.0)
let FUN5 = (comp FUN3 FUN4)

let RES = FUN5 ARG

entry main = RES
