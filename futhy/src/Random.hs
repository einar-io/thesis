module Random
  ( rndRel
  , vecToVal
  , rndVecInts
  , rndVecVals
  ) where

import Types
import Numeric.LinearAlgebra hiding ((|>))
import Flow
import System.Random

seed :: Seed
seed = 17981471111 -- Prime, but for no good reason.

rndVecInts :: Int -> [Int]
rndVecInts len
  = (randoms (mkStdGen seed) :: [Int])
  |> map abs
  |> map (`mod` 1000)
  |> take len

vecToVal :: [Int] -> Val
vecToVal = Tensor . map (Scalar . fromIntegral)

rndVecVals :: Int -> Val
rndVecVals len = rndVecInts len |> vecToVal

{-
rndVec :: Int -> Vector Double
rndVec length = randomVector seed Uniform length
-}

{-
--E.g. rndRel _size = List [(1,2), (0,2), (1,3), (5,6)]
rndRelStatic :: Int -> Rel
rndRelStatic _ =
  [1..10000]
    |> map (\x -> (x,x))
  |> List
-}

rndRel :: Int -> Rel
rndRel len =
  let indices = rndVecInts len
                  |> map abs
      values  = rndVecInts len
                  |> map abs
   in List <| zip indices values
