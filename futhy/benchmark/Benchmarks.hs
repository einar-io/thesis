module Benchmarks (main) where

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Interpretor (interpret)
import ReduceTests
import Test.Tasty.Bench
import Tests hiding (main)
import Matrix
import Random
import Types
import Flow

{-
countDown :: Int -> Int
countDown a
  | a == 0    = 1 -- print "bottom"
  | otherwise = countDown (a-1)


countDownGroup :: Benchmark
countDownGroup = bgroup "Counting Down"
  [ bench   "1000" $ nf countDown   1000
  , bench  "10000" $ nf countDown  10000
  , bench "100000" $ nf countDown 100000
  ]
-}

main :: IO ()
main = defaultMain
  [ reduce
  , lmap
  , zipBench
  -- wipFeatures
  -- interpretorGroup
  -- countDownGroup
  ]


-- goodCaseInterpretor :: (LFun, Val, Val) -> TestTree
benchInterpretor name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name <| nf (interpret lf) vin


reduce :: Benchmark
reduce = bgroup "Reduce"
  [ benchInterpretor "10000" (Red <| rndRel 10000) (rndVecVals 10000)
  ]

lmap :: Benchmark
lmap = bgroup "LMap"
  [ benchInterpretor "10000" (LMap (Scale 1.0)) (rndVecVals 10000)
  ]


zipBench :: Benchmark
zipBench = bgroup "Zip"
  [ benchInterpretor "10000" (Zip [Scale 1.0]) (Tensor [rndVecVals 10000])
  ]


-- [U] Make benchmarks work over [1..10] or similar.
-- [U] Zip benchmark
