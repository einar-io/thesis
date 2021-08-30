{-# LANGUAGE GADTs #-}

module Interpretor where
import Types

-- general implementation - outer product, no contraction
outer :: Val -> Val -> Val
outer (Scalar x) (Scalar y) = Scalar $ x * y
outer x@(Scalar _) (Tensor ys) = Tensor $ map (outer x) ys
outer (Tensor xs) y = Tensor $ map (outer y) xs
outer _ _ = undefined

applyOp :: BilOp -> Val -> Val -> Val
applyOp op a b = case op of
  Outer -> outer a b
  _ -> undefined

π1 :: Val -> Val
π1 (Pair l _) = l
π1 _ = undefined

π2 :: Val -> Val
π2 (Pair _ r) = r
π2 _ = undefined

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus left right = case (left, right) of
   (Scalar l, Scalar r)     -> Scalar $ l + r
   (Tensor ls, Tensor rs)   -> Tensor $ ls ++ rs  -- what is this `tensor plus` supposed to do?`
   (Pair ll lr, Pair rl rr) -> Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)
   _ -> undefined

interpret :: LFun -> Val -> Val
interpret f v = case f of
  LSec l op -> applyOp op l v
  RSec op r -> applyOp op v r
  Id        -> v
  --HatPlus {} -> undefined
  Lplus lfn rfn -> let { left  = interpret lfn v
                         ; right = interpret rfn v}
                         in left `vectorspacePlus` right
  Oplus lfn rfn -> Pair (interpret lfn $ π1 v) (interpret rfn $ π2 v)
  -- SunCross {}  -> undefined
  Scale s      -> outer (Scalar s) v
  Zero        -> Scalar 0.0
  Comp lfn rfn -> interpret lfn (interpret rfn v)
  Red _ -> undefined

eval :: LFun -> Val -> String
eval f v = show (interpret f v)
