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
outer   (Scalar xs)   (Scalar ys) = Scalar $ xs * ys
outer x@(Scalar  _)   (Tensor ys) = Tensor $ map (outer x) ys
outer   (Tensor xs) y@(Scalar  _) = Tensor $ map (outer y) xs
outer   (Tensor xs) y@(Tensor  _) = Tensor $ map (outer y) xs
outer _ _                         = error "outer case undefined"

failableZipWith :: (Show a, Show b) => (a -> b -> c) -> [a] -> [b] -> [c]
failableZipWith op as bs = if (length as == length bs)
                           then zipWith op as bs
                           else error $ "ZIPWITHFAILED: Different lengths of\n" <> show as <> "\nand\n" <> show bs

dotprod :: Val -> Val -> Val
dotprod x y = let (a, b) = (mkR1 x, mkR1 y)
              in Scalar $ sum $ failableZipWith (*) a b

vecmatmul :: Val -> Val -> Val
vecmatmul v m = let (u, mm) = (mkR1 v, mkR2 m)
                in mkT1 [sum $ failableZipWith (*) u mi | mi <- mm]

matvecmul :: Val -> Val -> Val
matvecmul x y = let (a, b) = (mkR2 x, mkR1 y)
                in mkT1 [sum $ failableZipWith (*) ai b | ai <- transpose a]

matmul :: Val -> Val -> Val
matmul x y = let (a, b) = (mkR2 x, mkR2 y)
             in mkT2 [[sum $ failableZipWith (*) ai bi | bi <- transpose b] | ai <- a]


errorfunction :: Val -> Val -> Val
errorfunction x y = let (a, b) = (mkR1 x, mkR1 y)
                        yminusout = Tensor $ failableZipWith (\ai bi -> Scalar $ ai - bi) a b
                    in dotprod yminusout yminusout

applyOp :: BilOp -> Val -> Val -> Val
applyOp Outer = outer
applyOp DotProd = dotprod
applyOp VecMatProd = vecmatmul
applyOp MatVecProd = matvecmul
applyOp MatrixMult = matmul
applyOp ErrorFunction = errorfunction


proj1 :: Val -> Val
proj1 (Pair l _) = l
proj1 _ = error "proj1 case undefined"

proj2 :: Val -> Val
proj2 (Pair _ r) = r
proj2 _ = error "proj2 case undefined"

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus Zero r@(Scalar _)         = r
vectorspacePlus l@(Scalar _) Zero         = l
vectorspacePlus (Scalar l) (Scalar r)     = Scalar $ l + r
vectorspacePlus (Tensor ls) (Tensor rs)   = Tensor $ zipWith vectorspacePlus ls rs
vectorspacePlus (Pair ll lr) (Pair rl rr) = Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)
vectorspacePlus _ _                       = error "vectorspacePlus case undefined"


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
interpret Id v  = Right v
interpret Dup v = Right $ Pair v v
interpret (Comp lfn rfn) v = do vr <- interpret rfn v
                                interpret lfn vr
interpret (Para lfn rfn) v = do vl <- interpret lfn $ proj1 v
                                vr <- interpret rfn $ proj2 v
                                Right $ Pair vl vr
interpret (LSec l op) v = Right $ applyOp op l v
interpret (RSec op r) v = Right $ applyOp op v r
interpret (Scale s) v   = Right $ outer (Scalar s) v
interpret KZero _       = Right Zero
interpret Fst v         = Right $ proj1 v
interpret Snd v         = Right $ proj2 v
interpret (Lplus lfn rfn) v = do vl <- interpret lfn v
                                 vr <- interpret rfn v
                                 Right $ vl `vectorspacePlus` vr

interpret (Red (List [])) _ = Left "Reduction relations can not be empty"
interpret (Red (List r )) v = let l = map snd r |> maximum |> Just in
                                    Right
                                    <| flip denseVecFromSparseVecL l
                                    <| SparseTensor
                                    <| reduce r v
interpret (Red r) v = Left <| "Invalid argument to Red.  rel: "
                      ++ show r
                      ++ " v: "
                      ++ show v

interpret Neg v = return (negate v)

interpret (LMap fn) (Tensor vs) = do vals <- mapM (interpret fn) vs
                                     return (Tensor vals)

interpret (LMap _) _ = Left "Must LMap over a Tensor (Vector)"

interpret (Zip fs) (Tensor vs) = if length fs == length vs
                                 then do vals <- zipWithM interpret fs vs
                                         return (Tensor vals)
                                 else Left "Invalid argument pair to Zip.  Lists of LFUNS and VLIST must have same length."

interpret (Zip _) _ = Left "Wrong use of zip!"

interpret Add (Pair Zero vr) = return vr
interpret Add (Pair vl Zero) = return vl
interpret Add (Pair (Scalar l) (Scalar r)) = return $ Scalar (l+r)
interpret Add (Pair (Tensor l) (Tensor r)) = if length l == length r
                                       then do vs <- zipWithM (\x y -> interpret Add (Pair x y)) l r
                                               return (Tensor vs)
                                       else Left "Invalid pair of tensors. Lists values do not have same length."
interpret Add _       = Left "Invalid argument to Add"
interpret (Prj _ _) _ = Left "Projection should have been desugared"

eval :: LFun -> Val -> String
eval f v = show (interpret f v)
