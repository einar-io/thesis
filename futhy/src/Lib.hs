module Lib where

type RealNumb = Float

data Var
  = Scalar RealNumb
  | Tensor [Var]
  | Pair Var Var
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

-- general implementation - outer product, no contraction
mult :: Var -> Var -> Var
mult (Scalar x) (Scalar y) = Scalar $ x * y
mult x@(Scalar _) (Tensor ys) = Tensor $ map (mult x) ys
mult (Tensor xs) y = Tensor $ map (mult y) xs
mult _ _ = undefined

applyOp :: BiOp -> Var -> Var -> Var
applyOp op a b = case op of
  Mult -> undefined
  Outer -> mult a b

interpret :: LFun -> Var -> Var
interpret f v = case f of
  Id -> v
  Comp f1 f2 -> interpret f2 (interpret f1 v)
  Scale r -> mult (Scalar r) v
  LSec v0 op -> applyOp op v0 v
  RSec op v0 -> applyOp op v v0

eval :: LFun -> Var -> String
eval f v = show (interpret f v)

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- error types!
data Error
  = Something String
  deriving (Show, Eq)
