{-# LANGUAGE GADTs #-}

module Lib where

type RealNumb = Float

data Val
  = Scalar RealNumb
  | Tensor [Val]
  | Pair Val Val
  deriving (Show, Eq)

-- These are listed as linear map expressions
-- [POPL, p. 21] 
data LFun -- expr
  = LSec Val BilOp
  | RSec BilOp Val
  | Id
  | HatPlus  LFun LFun -- Fritz says "lifted" addition sometimes
  | SunCross LFun LFun
  | Scale RealNumb
  | KConst Val
  | KZero
  | Comp LFun LFun
  deriving (Show, Eq)

-- These are bilinear operators
-- listed on [POPL, p. 20] 
data BilOp
  = ScalarProd
  | TensorProd
  | MatrixMult
  | DotProd
  | Mult
  | Outer
  deriving (Show, Eq)

-- general implementation - outer product, no contraction
mult :: Val -> Val -> Val
mult (Scalar x) (Scalar y) = Scalar $ x * y
mult x@(Scalar _) (Tensor ys) = Tensor $ map (mult x) ys
mult (Tensor xs) y = Tensor $ map (mult y) xs
mult _ _ = undefined

applyOp :: BilOp -> Val -> Val -> Val
applyOp op a b = case op of
  Mult -> undefined
  Outer -> mult a b

π1 :: Val -> Val
π1 (Pair l _) = l

π2 :: Val -> Val
π2 (Pair _ r) = r

vectorspacePlus :: Val -> Val -> Val
vectorspacePlus left right = case (left, right) of
   (Scalar l, Scalar r)     -> Scalar $ l + r
   (Tensor ls, Tensor rs)   -> Tensor $ ls ++ rs  -- what is this `tensor plus` supposed to do?`
   (Pair ll lr, Pair rl rr) -> Pair (ll `vectorspacePlus` rl) (lr `vectorspacePlus` rr)

interpret :: LFun -> Val -> Val
interpret f v = case f of
  LSec l op -> applyOp op l v
  RSec op r -> applyOp op v r
  Id        -> v
  --HatPlus {} -> undefined
  HatPlus lfn rfn -> let { left  = interpret lfn v
                         ; right = interpret rfn v}
                         in left `vectorspacePlus` right
  SunCross lfn rfn -> Pair (interpret lfn $ π1 v) (interpret rfn $ π2 v)
  SunCross {}  -> undefined
  Scale s      -> mult (Scalar s) v
  KConst _     -> undefined
  KZero        -> Scalar 0.0
  Comp lfn rfn -> interpret lfn (interpret rfn v)

eval :: LFun -> Val -> String
eval f v = show (interpret f v)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
