module Utils where
import Types

data Arity
  = Atom Int
  | APair Arity Arity
  deriving (Show, Eq)

ua :: Arity -> Int
ua a = case a of
  Atom i -> i
  _ -> undefined

hasArity :: Val -> Int -> Bool
hasArity v i = i == ua (getArity v)

getArity :: Val -> Arity
getArity v = case v of
  Scalar _ -> Atom 0
  Zero -> Atom 0
  Tensor (h:_) -> let (Atom i) = getArity h
                  in Atom (i+1)
  Pair v1 v2 -> APair (getArity v1) (getArity v2)
  Tensor [] -> undefined
