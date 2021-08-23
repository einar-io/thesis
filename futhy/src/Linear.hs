module Linear where

type R = Float

data V
  = S R
  | T [V]
  deriving (Show, Eq)

data LFun
  = Id
  | Scale R
  | Comp LFun LFun
  | LSec V BiOp
  | RSec BiOp V
  deriving (Show)

data BiOp = Mult
  deriving (Show)

int :: LFun -> V -> V
int Id v = v
int (Scale k) v = scale k v
  where
    scale k (S x) = S $ k * x
    scale k (T vs) = T $ map (scale k) vs
