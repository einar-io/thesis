{-# LANGUAGE GADTs #-}

module Interpreter where
import Types
import Control.Monad
import Utils
import Data.AssocList.List.Eq as AList
import Data.Maybe
import Flow
import Matrix

-- general implementation - outer product, no contraction
outer :: Val -> Val -> Val
outer x y = case (x,y) of
  (Scalar xs, Scalar ys) -> Scalar $ xs * ys
  (Scalar  _, Tensor ys) -> Tensor $ map (outer x) ys
  (Tensor xs, Scalar  _) -> Tensor $ map (outer y) xs
  (Tensor xs, Tensor  _) -> Tensor $ map (outer y) xs
  _ -> error "outer case undefined"

dotprod :: Val -> Val -> Val
dotprod x y = case (mkR1 x, mkR1 y) of
  (a, b) -> Scalar $ sum $ zipWith (*) a b

matmul :: Val -> Val -> Val
matmul x y = case (mkR2 x, mkR2 y) of
  (a, b) -> mkT2 [[sum $ zipWith (*) ai bi | bi <- transpose b] | ai <- a]

applyOp :: BilOp -> Val -> Val -> Val
applyOp op a b = case op of
  Outer -> outer a b
  DotProd -> dotprod a b
  MatrixMult -> matmul a b

proj1 :: Val -> Val
proj1 (Pair l _) = l
proj1 _ = error "proj1 case undefined"

proj2 :: Val -> Val
proj2 (Pair _ r) = r
proj2 _ = error "proj2 case undefined"

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus left right = case (left, right) of
   (Zero, r@(Scalar _))       -> r
   (l@(Scalar _), Zero)       -> l
   (Scalar l, Scalar r)       -> Scalar $ l + r
   (Tensor ls, Tensor rs)     -> Tensor $ zipWith vectorspacePlus ls rs
   (Pair ll lr, Pair rl rr)   -> Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)
   _ -> error "vectorspacePlus case undefined"


{- vecLookup, flipLookup, keepValues, compact, reduce are all helper functions to the reduce operator -}
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
  (KZero, _)     -> Right Zero
  (Fst, _)       -> Right $ proj1 v
  (Snd, _)       -> Right $ proj2 v
  (Lplus lfn rfn, _) -> do vl <- interpret lfn v
                           vr <- interpret rfn v
                           Right $ vl `vectorspacePlus` vr

  (Red (List []), _) -> return Zero
  (Red (List r ), _) -> let l = map snd r |> maximum |> Just in
                        Right
                        <| flip denseVecFromSparseVecL l
                        <| SparseTensor
                        <| reduce r v
  (Red r, _)         -> Left <| "Invalid argument to Red.  rel: "
                             ++ show r
                             ++ " v: "
                             ++ show v

  (Neg   , _) -> return (negate v)

  (LMap fn, Tensor vs) -> do vals <- mapM (interpret fn) vs
                             return (Tensor vals)
  (LMap _, _) -> error "Wrong use of LMap"

  (Zip fs, Tensor vs) -> if length fs == length vs
                        then do vals <- zipWithM interpret fs vs
                                return (Tensor vals)
                        else Left "Invalid argument pair to Zip.  Lists of LFUNS and VLIST must have same length."

  (Zip _, _) -> error "Wrong use of zip!"

  (Add, Pair Zero vr) -> return vr
  (Add, Pair vl Zero) -> return vl
  (Add, Pair (Scalar l) (Scalar r)) -> return $ Scalar (l+r)
  (Add, Pair (Tensor l) (Tensor r)) -> if length l == length r
                                       then do vs <- zipWithM (\x y -> interpret Add (Pair x y)) l r
                                               return (Tensor vs)
                                       else Left "Invalid pair of tensors. Lists values do not have same length."
  (Add, _) ->  Left "Invalid argument to Add"
  (Prj _ _, _) -> Left "Projection should have been desugared"

eval :: LFun -> Val -> String
eval f v = show (interpret f v)
