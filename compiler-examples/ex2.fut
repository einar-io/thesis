open import "lmaplib"
--Example program 2:
--ARG: (Scalar 10.0)
--FUN: (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dupe)
--RES: (Pair (Scalar 30.0, Scalar 70.0))
let ARG = [10.0f32]

let FUN1 = (sv 3.0)
let FUN2 = (sv 7.0)
let FUN3 = (para FUN1 FUN2)
let FUN4 = (dupe)
let FUN5 = (comp FUN3 FUN4)

let RES = FUN5 ARG -- ([30.0f32], [70.0f32])

entry main = RES
