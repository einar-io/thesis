module Preprocesser where
import Types

optimize :: LFun -> LFun
optimize lf = let lf2 = optimizeRun lf in
              if lf2 == lf then lf2 else optimize lf2

optimizeRun :: LFun -> LFun
optimizeRun lf = case lf of

  -- id simp
  Comp x Id -> optimizeRun x
  Comp Id x -> optimizeRun x
  Para Id Id -> Id

  -- restructuring comps
    -- make comp-trees flat and weighed to the right
    -- have as few Para as possible by combining them
      -- this way, we as few rules as possible
  Comp (Comp x y) z -> optimizeRun $ Comp x $ Comp y z

  Comp (Para x y) (Para a b) -> optimizeRun $ Para (Comp x a) (Comp y b)
  Comp (Para x y) (Comp (Para a b) c) -> optimizeRun $ Comp (Para (Comp x a) (Comp y b)) c


  -- combining operations

  -- Two scalar operations are the same as one
  Comp (Scale r1) (Scale r2) -> Scale $ r1*r2
  Comp (Scale r1) (Comp (Scale r2) x) -> optimizeRun $ Comp (Scale $ r1*r2) x

  -- otherwise ~
  Comp x y -> Comp (optimizeRun x) (optimizeRun y)
  Para x y -> Para (optimizeRun x) (optimizeRun y)

  -- leaves
  Lplus _ _ -> error "LPlus should be desugared!"
  Prj _ _ -> error "Projection should be desugared"

  Id -> lf
  Dup -> lf
  KZero -> lf
  Scale _ -> lf
  LSec _ _ -> lf
  RSec _ _ -> lf
  Fst -> lf
  Snd -> lf
  Red _ -> lf
  Add -> lf
  LMap _ -> lf
  Zip _ -> lf
  Neg -> lf


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

data LFunP -- expr
  = IdP
  | DupP
  | FstP
  | SndP
  | AddP
  | NegP
  | LSecP BilOp
  | RSecP BilOp
  | LMapP LFunP
  | ZipP LFunP
  | ParaP LFunP LFunP
  | CompP LFunP LFunP
  | RedP Rel
  deriving (Show, Eq)

extractLFunConsts :: LFun -> (LFunP, Val)
extractLFunConsts lf = case lf of
  Id            -> (IdP, Dummy)
  Dup           -> (DupP, Dummy)
  Fst           -> (FstP, Dummy)
  Snd           -> (SndP, Dummy)
  Add           -> (AddP, Dummy)
  Neg           -> (NegP, Dummy)
  Red r         -> (RedP r, Dummy)
  LSec v op     -> (LSecP op, v)
  RSec op v     -> (RSecP op, v)
  LMap lf2      -> let (lf2i, v) = extractLFunConsts lf2 in (LMapP lf2i, v)
  Para lf3 lf2  -> let (lf2i, v2) = extractLFunConsts lf2 in
                   let (lf3i, v3) = extractLFunConsts lf3 in
                   (ParaP lf3i lf2i, (Pair v3 v2))
  Comp lf3 lf2  -> let (lf2i, v2) = extractLFunConsts lf2 in
                   let (lf3i, v3) = extractLFunConsts lf3 in
                   (CompP lf3i lf2i, (Pair v3 v2))
                   -- assumes that the lfsis are identical
                   -- TODO: assert it!
  Zip lfs       -> let ((h:_), vis) = unzip $ map extractLFunConsts lfs in
                   (ZipP h, Tensor vis) -- HACK: doesnt keep tensor constraints

  Prj _ _       -> error "Proj should be desugared"
  Lplus _ _     -> error "LPlus should be desugared"
  KZero         -> error "KZero should be desugared"
  Scale _       -> error "Scale should be desugared"
