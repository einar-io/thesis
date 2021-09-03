module Types where

import Types.Internal()

type RealNumb = Float

data Val
  = Scalar RealNumb
  | Tensor [Val]
  | Pair Val Val
  deriving (Show, Eq)

-- These are listed as linear map expressions
-- [POPL, p. 21]
data LFun -- expr
  = Id
  | Dup
  | Zero
  | Scale RealNumb
  | LSec Val BilOp
  | RSec BilOp Val
  | Para LFun LFun -- more descriptive than Oplus
  | Comp LFun LFun
  | Prj Int Int
  | Lplus LFun LFun -- lifted addition
  | Red Rel
  | Add Int
  | LMap LFun
  | Zip [LFun]
  deriving (Show, Eq)

data Rel
  = List [(Int, Int)]
  | Func RelFun
  deriving (Show, Eq)

data RelFun
  = Const Int
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

type Derivative = Val
