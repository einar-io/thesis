module Matrix where

import Types
import Numeric.LinearAlgebra
import Data.Maybe

mtx = Tensor [Tensor [Scalar 2, Scalar 3], Tensor [Scalar 5, Scalar 7]]




type Shape = (Int, Int)

mkShape :: Int -> Int -> Maybe Shape
mkShape m n
  | m > 0 && n > 0 = Just (m, n)
  | otherwise      = Nothing




lfun2mtx :: LFun -> Shape -> Maybe (Matrix Double)
lfun2mtx Id (m, n) = if m == n
                                then Just (ident m)
                                else Nothing

lfun2mtx Dup (m, n) = undefined

lfun2mtx _ _ = undefined



shp = mkShape 5 5

main :: IO ()
main = disp 1 $ fromJust $ lfun2mtx Id (fromJust shp)
