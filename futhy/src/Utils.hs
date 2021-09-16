module Utils where
import Types

data Arity
  = Atom Int
  | APair Arity Arity
  deriving (Eq)

instance Show Arity where
  show a = case a of
    Atom i -> show i
    APair a3 a2 -> "(" <> show a3 <> ", " <> show a2 <> ")"

getArity :: Val -> Arity
getArity v = case v of
  Scalar _ -> Atom 0
  Zero -> Atom 0
  Tensor (h:_) -> let (Atom i) = getArity h
                  in Atom (i+1)
  Pair v1 v2 -> APair (getArity v1) (getArity v2)
  Tensor [] -> undefined
