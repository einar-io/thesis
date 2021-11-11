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

benchInterpretor :: String -> LFun -> Val -> Benchmark
benchInterpretor name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name <| nf (interpret lf) vin

main :: IO ()
main = defaultMain
  [ genBenchmarkGroups "Reduce" (genReduceBenchmark) 4
  , genBenchmarkGroups "LMap" (genLmapBenchmark) 5
  , genBenchmarkGroups "Zip" (genZipBenchmark) 5
  ]

genBenchmarkGroups :: String -> (Int -> Benchmark) -> Int ->  Benchmark
genBenchmarkGroups n f i = bgroup n $ map f $ powersof10 i

genLmapBenchmark :: Int -> Benchmark
genLmapBenchmark i = benchInterpretor (show i) (LMap (Scale 2.0)) (rndVecVals i)

genZipBenchmark :: Int -> Benchmark
genZipBenchmark i = benchInterpretor (show i) (Zip [Scale 2.0]) (Tensor [rndVecVals i])

genReduceBenchmark :: Int -> Benchmark
genReduceBenchmark i = benchInterpretor (show i) (Red <| rndRelCap i i (i `div` 4)) (rndVecVals i)

powersof10 i = [10 ^ ii | ii <- [1..i]]
