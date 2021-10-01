module Caramelizer where
import Types

caramelizeVal :: Val -> Val
caramelizeVal v = case v of
  _ -> v

caramelizeLFun :: LFun -> LFun
caramelizeLFun sfl = case sfl of
  Id            -> sfl
  Dup           -> sfl
  Neg           -> sfl
  KZero         -> LSec (Scalar 0.0) Outer
  Scale rn      -> LSec (Scalar rn) Outer
  LSec _ _      -> sfl
  RSec _ _      -> sfl
  Para sf2 sf1  -> Para (caramelizeLFun sf2) (caramelizeLFun sf1)
  Comp sf2 sf1  -> Comp (caramelizeLFun sf2) (caramelizeLFun sf1)
  Prj 2 1       -> Fst
  Prj 2 2       -> Snd
  Lplus sf2 sf1 -> Comp (Comp Add (Para (caramelizeLFun sf2) (caramelizeLFun sf1))) Dup
  Red _         -> sfl
  Add           -> sfl
  LMap sf1      -> LMap $ caramelizeLFun sf1
  Zip sfs       -> Zip $ map caramelizeLFun sfs
