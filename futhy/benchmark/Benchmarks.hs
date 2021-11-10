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

main :: IO ()
main = defaultMain
  [ reduce
  , lmap
  , zipBench
  -- wipFeatures
  -- interpretorGroup
  -- countDownGroup
  ]

benchInterpretor :: String -> LFun -> Val -> Benchmark
benchInterpretor name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name <| nf (interpret lf) vin

reduce :: Benchmark
reduce = bgroup "Reduce"
  [ reduce1000
  ]

reduce1000 :: Benchmark
reduce1000 =
  let vecLen = 1000
      relLen = 20
      maxIdx = vecLen
      maxVal = 256
   in benchInterpretor
        (show vecLen)
        (Red <| rndRelCap relLen maxIdx maxVal)
        (rndVecVals vecLen)

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
