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
  Prj _ _       -> error "invalid projection attempted desugar"
  Lplus sf2 sf1 -> Comp (Comp Add (Para (caramelizeLFun sf2) (caramelizeLFun sf1))) Dup
  Red _         -> sfl
  Add           -> sfl
  Fst           -> sfl
  Snd           -> sfl
  LMap sf1      -> LMap $ caramelizeLFun sf1
  Zip sfs       -> Zip $ map caramelizeLFun sfs


{- maybe TODO, if we desugar inner paras in zips
caramelizeValbak :: Val -> Val
caramelizeValbak v1 = case v1 of
  -- recursive case - list of >=1 elements
  Tensor ((Pair v3 v2):t@(_:_)) ->
    let (Pair (Tensor ls3) (Tensor ls2)) = caramelizeVal $ Tensor t
    in Pair (Tensor $ (caramelizeVal v3):ls3) (Tensor $ (caramelizeVal v2):ls2)

  -- base case - list of single element
  Tensor ((Pair v3 v2):_) -> Pair (Tensor [caramelizeVal v3]) (Tensor [caramelizeVal v2])

  Tensor ls -> Tensor $ map caramelizeVal ls
  Pair v3 v2 -> Pair (caramelizeVal v3) (caramelizeVal v2)
  Scalar _ -> v1
  Zero     -> v1

caramelizeLFunbak :: LFun -> LFun
caramelizeLFunbak sfl = case sfl of
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
  Prj _ _       -> error "invalid projection attempted desugar"
  Lplus sf2 sf1 -> Comp (Comp Add (Para (caramelizeLFun sf2) (caramelizeLFun sf1))) Dup
  Red _         -> sfl
  Add           -> sfl
  Fst           -> sfl
  Snd           -> sfl
  LMap sf1      -> LMap $ caramelizeLFun sf1
  Zip ls@(h:t@(_:_)) -> Zip $ map caramelizeLFun sfs
  Zip ls@(h:_) -> Zip $ map caramelizeLFun sfs
-}
