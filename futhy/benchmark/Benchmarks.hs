
-- module Benchmarks (main) where

import Interpretor (interpret)
import ReduceTests

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Test.Tasty.Bench
import Tests hiding (main)


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


main :: IO ()
main = defaultMain
  [ countDownGroup
  , interpretorGroup
  --, wipFeatures
  ]

someRandomVal = undefined

-- goodCaseInterpretor :: (LFun, Val, Val) -> TestTree
goodCaseInterpretor params = let (lf, vin, _) = caramelizeTestParams params in
  bench  "name" $ nf (interpret lf) vin

interpretorGroup :: Benchmark
interpretorGroup = bgroup "Interpretor bench"
 $ map goodCaseInterpretor (map (\(_, a, b, c) -> (a, b, c)) reduceTests)


--futhyBench = bench "Futhy" runAllTests

