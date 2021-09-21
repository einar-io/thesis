{-# LANGUAGE GADTs #-}

module Interpretor where
import Types
import Control.Monad
import Utils
import Data.List
import Data.AssocList.List.Eq as AList
import Data.Maybe
import Flow
import Control.Applicative

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

{- vecLookup, flipLookup, keepValues, reduce are all helper functions to the reduce operator -}
{- Safely dereferences a vector unlike the (!!) operator. -}
vecLookup :: Index -> Val -> Maybe Val
vecLookup idx (Tensor v) = nth idx v
vecLookup _   _          = Nothing

{- This is needed for the red operation -}
flipLookup :: (Index, a) -> Val -> (a, Maybe Val)
flipLookup (x, y) vs = (y, vecLookup x vs)

{- Takes an AList and merges values of duplicate keys -}
compact :: Eq k => [(k, [v])] -> [(k, v)] -> [(k, [v])]
compact acc []              = acc
compact acc allkv@((k,_):_) =
  let (vs, rest) = AList.partition k allkv
   in compact ((k,vs):acc) rest

{- Get rid of Nothings and unwrap the Justs. -}
keepValues :: (a, [Maybe b]) -> (a, [b])
keepValues (a, bs) =
  let vs = filter isJust bs
           |> catMaybes
   in (a, vs)

reduce :: Eq a => [(Index, a)] -> Val -> [(a, Val)]
reduce r v = map (`flipLookup` v) r
              |> compact []
              |> map keepValues
              |> filter (\(_,y) -> y /= [])
              |> map (\(x,y) -> (x,sum y))

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
  (Lplus lfn rfn, _) -> do vl <- interpret lfn v
                           vr <- interpret rfn v
                           Right $ vl `vectorspacePlus` vr

  (Red (List []), _) -> return Zero
  (Red (List r ), _) -> Right $ SparseTensor $ reduce r v

  (Neg   , _) -> Right $ negate v

  (LMap _, _) -> Left "Invalid argument to LMap"

  (Zip fs, Tensor vs) -> case length fs `compare` length vs of
                           EQ -> do vals <- zipWithM interpret fs vs
                                    Right $ Tensor vals
                           _  -> Left "Invalid argument pair to Zip.  Lists of LFUNS and list of values must have same length."

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
