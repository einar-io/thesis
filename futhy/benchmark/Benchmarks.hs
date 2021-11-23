module Benchmarks (main) where

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Interpreter (interpret)
--import ReduceTests
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.Maybe
import Data.Maybe
import Data.Either
import Data.Either
import Test.Tasty.Bench
import Tests hiding (main)
--import Matrix
import Random
import Types hiding (runs)
import Flow
import Executer


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
  , reduce
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

powersof10 :: (Num a, Integral b) => b -> [a]
powersof10 i = [10 ^ ii | ii <- [1..i]]


{- Old-flavour benchmarks for testing GPU -}

benchCompiler :: String -> LFun -> Val -> Benchmark
benchCompiler name lf1 vin1 =
  let (lf, vin, _vout) = caramelizeTestParams (lf1, vin1, Zero)
   in bench name
      <| nfIO
      <| runStrArg (show lf) OPENCL (show vin)

reduce :: Benchmark
reduce = bgroup "Reduce"
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

reduce1000Cnew :: IO (CommandExecution Result)
reduce1000Cnew =
  let vecLen = 100000
      relLen = 200
      maxIdx = vecLen
      maxVal = 256
      runs = 5
   in benchmark
        "einartest"
        (Red <| rndRelCap relLen maxIdx maxVal)
        (rndVecVals vecLen)
        C
        runs

main :: IO ()
main = do
  json <- benchAndGenPlot reduce1000Cnew
  return ()

benchAndGenPlot :: IO (CommandExecution Result) -> IO Json
benchAndGenPlot bench = do
  cmp <- bench
  case cmp of
    Left _ -> return ""
    Right (CommandResult log) -> return $ mjson log

plotJson :: Json -> IO ()
plotJson = putStrLn
-- Do something with the json here, e.g. `matplotlib json`.
