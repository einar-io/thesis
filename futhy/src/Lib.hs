module Lib where

import Prelude

type RealNumb = Float

data Var
  = Scalar RealNumb
  | Tensor [Var]
  deriving (Show, Eq)

data LFun
  = Id
  | Scale RealNumb
  | Comp LFun LFun
  | LSec Var BiOp
  | RSec BiOp Var
  deriving (Show, Eq)

data BiOp
  = Mult
  | Outer
  deriving (Show, Eq)

outer :: Var -> Var -> Var
outer (Scalar x) (Scalar y) = Scalar $ x * y
outer x@(Scalar _) (Tensor ys) = Tensor $ map (outer x) ys
outer (Tensor xs) v = Tensor $ map (outer v) xs

-- dotproduct :: Var -> Var -> Var
-- dotproduct (Tensor a) (Tensor b) = Scalar $ sum $ a * b

-- works for dot product, matrix multiplication, 'binary tensor contraction' at https://www.tensors.net/p-tutorial-1
-- pairs last dimension of first variable with first dimension of second tensor
-- please dont use it on scalars...
generalContraction :: Var -> Var -> Var
generalContraction a b = case (a,b) of
  (Tensor x, Tensor y) -> undefined
  _ -> undefined -- error

applyOp :: BiOp -> Var -> Var -> Var
applyOp op a b = case op of
  Mult -> undefined
  Outer -> outer a b

interpret :: LFun -> Var -> Var
interpret f v = case f of
  Id -> v
  Comp f1 f2 -> interpret f2 (interpret f1 v)
  Scale r -> outer (Scalar r) v
  LSec v0 op -> applyOp op v0 v
  RSec op v0 -> applyOp op v v0

eval :: LFun -> Var -> String
eval f v = show (interpret f v)






someFunc :: IO ()
someFunc = putStrLn "someFunc"
