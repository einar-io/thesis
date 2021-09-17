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

-- Source https://hackage.haskell.org/package/tidal-1.7.8/docs/src/Sound.Tidal.Utils.html#nth
{- | Safer version of !! --}
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs
