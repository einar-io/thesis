module Types where

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
  | Lplus LFun LFun -- lifted addition
  | Oplus LFun LFun
  | Scale RealNumb
  | Zero
  | Para LFun LFun -- apply to each of pair
  | Comp LFun LFun
  | Red Rel
  | Dup
  | Add Int
  | Prj Int Int
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
