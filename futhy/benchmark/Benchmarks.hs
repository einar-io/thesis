module Benchmarks (main) where

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Interpreter (interpret)
--import ReduceTests
--import Control.Monad
--import Control.Monad.Except
--import Control.Monad.Trans.Maybe
--import Data.Maybe
import Data.Either
import Test.Tasty.Bench
import Tests hiding (main)
--import Matrix
import Random
import Types hiding (runs)
import Utils
import Flow
import Executer
import Plot (savePlot)
import Json (json2series)

benchInterpretor :: String -> LFun -> Val -> Benchmark
benchInterpretor name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name <| nf (interpret lf) vin

mainOld :: IO ()
mainOld = defaultMain
  [ genBs "Reduce" genReduceBenchmark 4
  , genBs "Scale" genScaleBenchmark 5
  , genBs "LMap" genLmapBenchmark 5
  , genBs "Zip" genZipBenchmark 5
  --, reduce
  ]

genBs :: String -> (Int -> Benchmark) -> Int ->  Benchmark
genBs n f i = bgroup n $ map f $ powersof10 i

genScaleBenchmark :: Int -> Benchmark
genScaleBenchmark i = benchInterpretor (show i) (Scale 2.0) (rndVecVals i)

genLmapBenchmark :: Int -> Benchmark
genLmapBenchmark i = benchInterpretor (show i) (LMap (Scale 2.0)) (rndVecVals i)

genZipBenchmark :: Int -> Benchmark
genZipBenchmark i = benchInterpretor (show i) (Zip [Scale 2.0]) (Tensor [rndVecVals i])

genReduceBenchmark :: Int -> Benchmark
genReduceBenchmark i = benchInterpretor (show i) (Red <| rndRelCap i i (i `div` 4)) (rndVecVals i)

{- Old-flavour benchmarks for testing GPU -}

benchCompiler :: String -> LFun -> Val -> Benchmark
benchCompiler name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name
      <| nfIO
      <| runStrArg (show lf) OPENCL (show vin)

reduce1 :: Benchmark
reduce1 = bgroup "Reduce"
  [ reduce1000
  ]

reduce1000 :: Benchmark
reduce1000 =
  let vecLen = 100000
      relLen = 20
      maxIdx = vecLen
      maxVal = 256
   in benchInterpretor
        (show vecLen ++ " Interpretor")
        (Red <| rndRelCap relLen maxIdx maxVal)
        (rndVecVals vecLen)


{- New-flavour benchmarks for testing GPU -}

reduce :: Bench
reduce name i =
  let vecLen = i
      relLen = (20 *) . floor . log <| (fromIntegral vecLen :: Double)
      maxIdx = vecLen
      maxVal = 256
      runs = 10
   in benchmark
        name
        (Red <| rndRelCap relLen maxIdx maxVal)
        (rndVecVals vecLen)
        C
        runs


genBenchmarks :: String -> Bench -> Int -> IO PlotData
genBenchmarks name bench n = do
  let vecSizes = map (* 1) $ powersof2 n
  cexs <- mapM (\i -> bench (sizename i) i) vecSizes
  let jsons = map (json . getLog) (rights cexs)
  seriess <- mapM json2series jsons
  return (name, vecSizes, seriess)
    where sizename i = name ++ ".i=" ++ show i

main :: IO ()
main = do
  genBenchmarks "Reduce" reduce 20 >>= savePlot
  -- genBenchmarks "Scale" scale 10 >>= savePlot
  -- genBenchmarks "LMap"  lmap 10  >>= savePlot

