module Utils where
import Types

data Arity
  = Atom Int
  | P Arity Arity
  deriving (Show, Eq)

ua :: Arity -> Int
ua a = case a of
  Atom i -> i
  _ -> undefined

hasArity :: Val -> Int -> Bool
hasArity v i = i == (ua (val_arity v))

val_arity :: Val -> Arity
val_arity v = case v of
  Scalar _ -> Atom 0
  Tensor (h:_) -> let (Atom i) = val_arity h
                  in Atom (i+1)
  Pair v1 v2 -> P (val_arity v1) (val_arity v2)
  Tensor [] -> undefined
