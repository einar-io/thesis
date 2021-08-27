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
  | HatPlus  LFun LFun -- Fritz says "lifted" addition sometimes
  | SunCross LFun LFun
  | Scale RealNumb
  | KZero
  | Comp LFun LFun
  | Red Rel
  deriving (Show, Eq)

data Rel
  = Permute [(Int, Int)]
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

-- error types!
data Error
  = Something String
  deriving (Show, Eq)
