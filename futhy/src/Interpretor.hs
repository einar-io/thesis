{-# LANGUAGE GADTs #-}

module Interpretor where
import Types
import Control.Monad
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

proj1 :: Val -> Val
proj1 (Pair l _) = l
proj1 _ = undefined

proj2 :: Val -> Val
proj2 (Pair _ r) = r
proj2 _ = undefined

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus left right = case (left, right) of
   (Scalar l, Scalar r)     -> Scalar $ l + r
   (Tensor ls, Tensor rs)   -> Tensor $ zipWith vectorspacePlus ls rs
   (Pair ll lr, Pair rl rr) -> Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)
   _ -> undefined


interpret :: LFun -> Val -> InterpretorOutput Val
interpret f v = case (f, v) of
  (Id,  _) -> Right v
  (Dup, _) -> Right $ Pair v v
  (Comp lfn rfn, _) -> do vr <- interpret rfn v
                          interpret lfn vr
  (Para lfn rfn, _) -> do vl <- interpret lfn $ proj1 v
                          vr <- interpret rfn $ proj2 v
                          Right $ Pair vl vr
  (LSec l op, _) -> Right $ applyOp op l v
  (RSec op r, _) -> Right $ applyOp op v r
  (Scale s, _)   -> Right $ outer (Scalar s) v
  (KZero, _)     -> Right $ Scalar 0.0 -- can this be done better?
  (Prj 2 1, _) -> Right $ proj1 v
  (Prj 2 2, _) -> Right $ proj2 v
  (Prj _ _, _) -> Left "Invalid argument to Prj"
  (Lplus lfn rfn, _) -> {- let { left  = interpret lfn v
                            ; right = interpret rfn v }
                            in Right $ left `vectorspacePlus` right -}
                        do
                          vl <- interpret lfn v
                          vr <- interpret rfn v
                          Right $ vl `vectorspacePlus` vr
  (Red  _, _) -> Left "Invalid argument to Red"
  (LMap _, _) -> Left "Invalid argument to LMap"
  (Zip  _, _) -> Left "Invalid argument to Zip"
  (Neg   , _) -> Left "Invalid argument to Neg"

  (Add, Pair Zero vr) -> Right vr
  (Add, Pair vl Zero) -> Right vl
  (Add, Pair (Scalar l) (Scalar r)) -> Right $ Scalar (l+r)
  (Add, Pair (Tensor l) (Tensor r)) -> case length l `compare` length r of
                                         EQ -> do vs <- zipWithM (\x y -> interpret Add (Pair x y)) l r
                                                  Right $ Tensor vs
                                         _  -> Left "Invalid pair of tensors. Lists values do not have same length."
  (Add, _) ->  Left "Invalid argument to Add"


eval :: LFun -> Val -> String
eval f v = show (interpret f v)
