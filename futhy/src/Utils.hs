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

mkT1 :: [RealNumber] -> Val
mkT1 x = Tensor $ map Scalar x

mkT2 :: [[RealNumber]] -> Val
mkT2 x = Tensor $ map mkT1 x

mkR0 :: Val -> RealNumber
mkR0 (Scalar x) = x
mkR0 _ = undefined

mkR1 :: Val -> [RealNumber]
mkR1 (Tensor x@(Scalar _: _)) = map mkR0 x
mkR1 _ = undefined

mkR2 :: Val -> [[RealNumber]]
mkR2 (Tensor x@(Tensor (Scalar _: _) : _)) = map mkR1 x
mkR2 _ = undefined

--- from https://hackage.haskell.org/package/base-4.15.0.0/docs/src/Data-OldList.html#transpose
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
-- Source https://hackage.haskell.org/package/tidal-1.7.8/docs/src/Sound.Tidal.Utils.html#nth
{- | Safer version of !! --}
nth :: Int -> [a] -> Maybe a
nth _ []       = Nothing
nth 0 (x : _)  = Just x
nth n (_ : xs) = nth (n - 1) xs
