module Optimizer where
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
