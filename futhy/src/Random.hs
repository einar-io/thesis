module Random
  ( vecToVal
  , rndVecInts
  , rndVecRealNumbers
  , rndVecVals
  , rndRelCap
  ) where

import Types
import System.Random
import Numeric.LinearAlgebra hiding ((|>))
import Flow

seed :: Seed
seed = 17981471111 -- unnecessarily prime

rndVecInts :: Int -> [Int]
rndVecInts len = take len <| randoms (mkStdGen seed) :: [Int]

rndVecRealNumbers :: Int -> [RealNumber]
rndVecRealNumbers len = take len <| randoms (mkStdGen seed) :: [RealNumber]

vecToVal :: [RealNumber] -> Val
vecToVal = Tensor . map Scalar

rndVecVals :: Int -> Val
rndVecVals = vecToVal . rndVecRealNumbers

rndRelCap :: Int -> Int -> Int -> Rel
rndRelCap len maxIdx maxVal =
  let indices = rndVecInts len
                |> map abs
                |> map (`mod` maxIdx)
      values  = rndVecInts len
                |> map abs
                |> map (`mod` maxVal)
   in List <| zip indices values

{-
rndVec :: Int -> Vector Double
rndVec length = randomVector seed Uniform length

rndRel :: Int -> Rel
rndRel length =
  let indices = rndVecInts length
                  |> map abs
      values  = rndVecInts length
                  |> map abs
   in List <| zip indices values
-}

