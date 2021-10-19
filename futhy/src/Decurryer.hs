module Decurryer where
import Types

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

dummyValue :: Val
dummyValue = Scalar 9999999999

decurryLFun :: LFun -> (LFunP, Val)
decurryLFun lf = case lf of
  Id            -> (IdP, dummyValue)
  Dup           -> (DupP, dummyValue)
  Fst           -> (FstP, dummyValue)
  Snd           -> (SndP, dummyValue)
  Add           -> (AddP, dummyValue)
  Neg           -> (NegP, dummyValue)
  Red r         -> (RedP r, dummyValue)
  LSec v op     -> (LSecP op, v)
  RSec op v     -> (RSecP op, v)
  LMap lf2      -> let (lf2i, v) = decurryLFun lf2 in (LMapP lf2i, v)
  Para lf3 lf2  -> let (lf2i, v2) = decurryLFun lf2 in
                   let (lf3i, v3) = decurryLFun lf3 in
                   (ParaP lf3i lf2i, (Pair v3 v2))
  Comp lf3 lf2  -> let (lf2i, v2) = decurryLFun lf2 in
                   let (lf3i, v3) = decurryLFun lf3 in
                   (CompP lf3i lf2i, (Pair v3 v2))
                   -- assumes that the lfsis are identical
                   -- TODO: assert it!
  Zip lfs       -> let ((h:_), vis) = unzip $ map decurryLFun lfs in
                   (ZipP h, Tensor vis) -- HACK: doesnt keep tensor constraints

  Prj _ _       -> error "Proj should be desugared"
  Lplus _ _     -> error "LPlus should be desugared"
  KZero         -> error "KZero should be desugared"
  Scale _       -> error "Scale should be desugared"
