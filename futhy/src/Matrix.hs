{-# LANGUAGE LambdaCase #-}

module Matrix
  ( Shape
  , LeftRightMultipliers
  , lfun2mtcs
  , tnsr2mtx
  , compaction
  -- , denseVecFromSparseVec
  , denseVecFromSparseVecL
  , (@@) -- matmul
  , genBasis
  )
  where

import Prelude hiding ((<>))
import Types
import Numeric.LinearAlgebra hiding ((|>))
import Flow
import Data.AssocList

type Shape = (Int, Int)
type LeftRightMultipliers = ([Matrix RealNumber], [Matrix RealNumber])


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

scalar2real :: Val -> RealNumber
scalar2real (Scalar s) = s
scalar2real _ = error "Called scalar2real with something other than a scalar"

tensor2reals :: Val -> [RealNumber]
tensor2reals (Tensor vs) = map scalar2real vs
tensor2reals _ = error "Called tensor2reals with something other than a tensor fo scalars"

tnsr2mtx :: Val -> Shape -> Matrix RealNumber
-- Tensor must be of rank 2 to be trivially convertable to a matrix.
tnsr2mtx (Tensor ts@(Tensor _:_)) _shp = matrix (length ts) (ts |> map tensor2reals |> concat)
-- tnsr2mtx (SparseTensor ss@(SparseTensor kvs)) shp = tnsr2mtx (sparse2dense ss) shp
tnsr2mtx _ _ = error "Called tnsr2mtx with something not a tensor of tensors of scalars"

-- injects
inl :: Matrix RealNumber -> [LeftRightMultipliers]
inl l = [([l],[])]

inr :: Matrix RealNumber -> [LeftRightMultipliers]
inr r = [([],[r])]

diagmtx :: RealNumber -> Int -> [LeftRightMultipliers]
diagmtx s m = inl <| scale s <| ident m

lfun2mtcs :: LFun -> Shape -> [LeftRightMultipliers]
{- Note that matmult with Id, KZero, Scale s and Neg are commutative, so it
 - does not matter which side we multiply from. -}
lfun2mtcs Id        (m, _n) = diagmtx   1  m
lfun2mtcs (Scale s) (m, _n) = diagmtx   s  m
lfun2mtcs Neg       (m, _n) = diagmtx (-1) m
lfun2mtcs KZero     (m, _n) = diagmtx   0  m

lfun2mtcs (LSec m bilop) shp =
  let multiplier = inl <| tnsr2mtx m shp
  in case bilop of
  Outer      -> undefined
  MatrixMult -> multiplier
  DotProd    -> multiplier

lfun2mtcs (RSec bilop m) shp =
  let multiplier = inr <| tnsr2mtx m shp
  in case bilop of
  Outer      -> undefined
  MatrixMult -> multiplier
  DotProd    -> multiplier

lfun2mtcs (Comp l r) shp =
  let [(ll,lr)] = lfun2mtcs l shp
      [(rl,rr)] = lfun2mtcs r shp
   in [(ll ++ rl, lr ++ rr)]

lfun2mtcs (Zip lfs) shp = concatMap (`lfun2mtcs` shp) lfs

lfun2mtcs (LMap _lf) _shp = undefined -- Postponed.  Needs length operated on.

lfun2mtcs (Para _fl _fr) _shp = undefined -- this requires a design for tuples.

lfun2mtcs _ _ = undefined

-- We can only reduce, when the matrix multiplication is on the same side.
-- e.g. we can reduce ABv to (A*B)v compile time.
-- e.g. we cannot reduce AvB further.
-- This step should preferably be done on the GPU for performance reasons.
compaction :: LeftRightMultipliers -> Shape -> LeftRightMultipliers
compaction (lfns, rfns) (m, _n) =
  let fmm = foldr (@@) (ident m)
   in ([fmm lfns], [fmm rfns])

buildVec :: AssocList Int Val -> Int -> Int -> [Val]
buildVec al len i
  | i == len = []
  | otherwise   = lookupDef 0 i al : buildVec al len (i+1)

denseVecFromSparseVecL :: Val -> Maybe Int -> Val
denseVecFromSparseVecL (SparseTensor []) _ = undefined
denseVecFromSparseVecL (SparseTensor alist) Nothing =
  let len = maximum <| map fst alist
   in Tensor <| buildVec alist (len + 1) 0
denseVecFromSparseVecL (SparseTensor alist) (Just len) =
   Tensor <| buildVec alist (len + 1) 0
denseVecFromSparseVecL _ _ = undefined

oneHot :: Int -> Int -> [Int]
oneHot l n
  | n < 1 = undefined
  | l < n = undefined
  | otherwise = before ++ [1] ++ after
  where  before = repeat 0 |> take (n-1)
         after  = repeat 0 |> take (l-n)

genBasis :: [Int] -> [[Int]]
genBasis [r] = [ oneHot r n | n <- [1..r] ]
genBasis _ = undefined
