{-# LANGUAGE GADTs #-}

module Interpretor where
import Types
-- import Utils

-- general implementation - outer product, no contraction
outer :: Val -> Val -> Val
outer x y = case (x,y) of
  (Scalar xs, Scalar ys) -> Scalar $ xs * ys
  (Scalar  _, Tensor ys) -> Tensor $ map (outer x) ys
  (Tensor xs, Scalar  _) -> Tensor $ map (outer y) xs
  (Tensor xs, Tensor  _) -> Tensor $ map (outer y) xs
  _ -> undefined

dotprod :: Val -> Val -> Val
dotprod _ _ = undefined --Scalar $ foldr (+) 0.0 $ map (\(Scalar aa, Scalar bb) -> aa*bb) $ zip a b

applyOp :: BilOp -> Val -> Val -> Val
applyOp op a b = case op of
  Outer -> outer a b
  DotProd -> dotprod a b
  ScalarProd -> undefined
  TensorProd -> undefined
  MatrixMult -> undefined
  Mult -> undefined

π1 :: Val -> Val
π1 (Pair l _) = l
π1 _ = undefined

π2 :: Val -> Val
π2 (Pair _ r) = r
π2 _ = undefined

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus left right = case (left, right) of
   (Scalar l, Scalar r)     -> Scalar $ l + r
   (Tensor ls, Tensor rs)   -> Tensor $ map (\(x,y) -> vectorspacePlus x y) $ zip ls rs
   (Pair ll lr, Pair rl rr) -> Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)
   _ -> undefined

interpret :: LFun -> Val -> Val
interpret f v = case f of
  Id -> v
  Dup -> Pair v v
  Comp lfn rfn -> interpret lfn (interpret rfn v)
  Para lfn rfn -> Pair (interpret lfn $ π1 v) (interpret rfn $ π2 v)
  LSec l op -> applyOp op l v
  RSec op r -> applyOp op v r
  Scale s -> outer (Scalar s) v
  Zero -> Scalar 0.0 -- can this be done better?
  Prj 2 1 -> π1 v
  Prj 2 2 -> π2 v
  Prj _ _ -> undefined
  Lplus lfn rfn -> let { left  = interpret lfn v
                       ; right = interpret rfn v}
                       in left `vectorspacePlus` right
  Red _ -> undefined
  Add _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined

eval :: LFun -> Val -> String
eval f v = show (interpret f v)
