{-# LANGUAGE LambdaCase #-}

module Matrix
  ( Shape
  , LeftRightMultipliers
  , lfun2mtcs
  , tnsr2mtx
  , compaction
  , (@@)
  )
  where

import qualified Prelude
import Prelude hiding ((<>))
import Types
import Numeric.LinearAlgebra hiding ((|>))
import Data.Maybe
import Flow
import Control.Monad

{- MODULE DESCRIPTION
 - This module contains machinery for converting LFuns to matrices
 - of the format provided by the `hmatrix` package wrapped in a pair of lists,
 - that describes which side the matrix should be multiplied on to `Val` V from.
 -
 - E.g.
 - lfn :: LFun -> [LeftRightMultipliers]
 - lfn = \v -> LRLRLLRv ===> [([LLLL],[RRR])]
 - This can then later be applied as (LLLLL)v(RRR).
 - Compaction can reduce this as follows: (LLLLL)v(RRR) -> LvR.
 - We want this to happen on the GPU, but it is working here for now, see `compaction`.
   -
 - Note a `Shape` is required to construct matrices of the correct size.
 - STATUS it implements the arity-preserving LFuns only.
 - It compacts the list of matrices on the CPU.
 - Future work:
 - 1. Implement the arity-changing LFuns.
 -}

-- Matrix multiplication operator
(@@) :: Matrix RealNumber -> Matrix RealNumber -> Matrix RealNumber
(@@) = (Numeric.LinearAlgebra.<>)

type Shape = (Int, Int)
type LeftRightMultipliers = ([Matrix RealNumber], [Matrix RealNumber])

scalar2real :: Val -> RealNumber
scalar2real (Scalar s) = s

tensor2reals :: Val -> [RealNumber]
tensor2reals (Tensor vs) = map scalar2real vs

tnsr2mtx :: Val -> Shape -> Matrix RealNumber
-- Tensor must be of rank 2 to be trivially convertable to a matrix.
tnsr2mtx (Tensor ts@(Tensor _:_)) (m, n) = matrix (length ts) (ts |> map tensor2reals |> concat)
tnsr2mtx _ _ = undefined

inl :: Matrix RealNumber -> [LeftRightMultipliers]
inl l = [([l],[])]

inr :: Matrix RealNumber -> [LeftRightMultipliers]
inr r = [([],[r])]

diagm s m = inl <| scale s <| ident m

lfun2mtcs :: LFun -> Shape -> [LeftRightMultipliers]
{- Note that matmult with Id, KZero, Scale s and Neg are commutative, so it
 - does not matter which side we multiply from. -}
lfun2mtcs lf@Id        (m, _n) = diagm   1  m
lfun2mtcs lf@(Scale s) (m, _n) = diagm   s  m
lfun2mtcs lf@Neg       (m, _n) = diagm (-1) m
lfun2mtcs lf@KZero     (m, _n) = diagm   0  m

lfun2mtcs lf@(LSec m bilop) shp =
  let multiplier = inl <| tnsr2mtx m shp
  in case bilop of
  Outer      -> undefined
  MatrixMult -> multiplier
  DotProd    -> multiplier

lfun2mtcs lf@(RSec bilop m) shp =
  let multiplier = inr <| tnsr2mtx m shp
  in case bilop of
  Outer      -> undefined
  MatrixMult -> multiplier
  DotProd    -> multiplier

lfun2mtcs lf@(Comp l r) shp =
  let [(ll,lr)] = lfun2mtcs l shp
      [(rl,rr)] = lfun2mtcs r shp
   in [(ll ++ rl, lr ++ rr)]

lfun2mtcs (Zip lfs) shp = concatMap (`lfun2mtcs` shp) lfs

lfun2mtcs (LMap lf) shp = undefined -- Postponed.  Needs length operated on.

lfun2mtcs (Para fl fr) shp = undefined -- this requires a design for tuples.

lfun2mtcs _ _ = undefined

-- We can only reduce, when the matrix multiplication is on the same side.
-- e.g. we can reduce ABv to (A*B)v compile time.
-- e.g. we cannot reduce AvB further.
-- This step should preferably be done on the GPU for performance reasons.
compaction :: LeftRightMultipliers -> Shape -> LeftRightMultipliers
compaction (lfns, rfns) (m, _n) =
  let fmm = foldr (@@) (ident m)
   in ([fmm lfns], [fmm rfns])
