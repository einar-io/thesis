module Benchmarks (main) where

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Interpreter (interpret)
import Data.Either
--import Test.Tasty.Bench
import Tests hiding (main)
import Random
import Types hiding (runs)
import Utils
import Flow
import Executer
import Plot (plotMeasurements)
import JSON (json2series)
import Dataset
import Matrix
import Control.Monad


{- 
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


-}

{- New-flavour benchmarks for testing GPU -}
scaleSym :: Bench
scaleSym backend runs inputLen =
  let lfn = Scale 7.0
   in benchmark "ScaleSym" inputLen backend runs lfn (rndVecVals inputLen)

scaleMtx :: Bench
scaleMtx backend runs inputLen =
  let mtx = getMatrixRep (Scale 59.0) [inputLen]
      lfn = LSec mtx MatrixMult
   in benchmark "ScaleMtx" inputLen backend runs lfn (rndVecVals inputLen)


{-
lmapB :: Bench
lmapB name dataset backend vecLen runs  = benchmark name dataset backend runs (LMap (Scale 11.0)) (rndVecVals vecLen)

zipB :: Bench
zipB name dataset backend vecLen runs   = benchmark name dataset backend runs (Zip [Scale 17.0]) (Tensor [rndVecVals vecLen])

reduceB :: Bench
reduceB name dataset backend vecLen runs =
  let relLen = 100 -- (20 *) . floor . log <| (fromIntegral vecLen :: Double)
      maxIdx = vecLen
      maxVal = 100
   in benchmark name dataset backend runs (Red <| rndRelCap relLen maxIdx maxVal) (rndVecVals vecLen)
-}



doBenchmarks :: Bench -> Backend -> Runs -> OOMs -> IO ([Int], [Double])
doBenchmarks bench backend runs ooms = do
  let inputLens = powersof2 ooms
  cexs <- mapM (bench backend runs) inputLens
  print ( "LENGHT: "  ++ (show . length $ cexs)
    ++ "; noRights: " ++ (show . length . rights $ cexs)
    ++ "; noLefts: "  ++ (show . length . lefts $ cexs ))
  print cexs
  guard (length (rights cexs) == length inputLens)

  let jsons = map (json . getLog) (rights cexs)
  seriess <- mapM json2series jsons -- we don't need to do this in IO
  let series = map minimum seriess -- this is important
  return (inputLens, series)

main :: IO ()
main = do

  let backend = C
  let oom = (1, 21) -- ordersOfMagnitude of 2 of the datasets.  Should be more than 10
  let noRuns = 1

  initDatasets oom

  measurementsScaleSym <- doBenchmarks scaleSym backend noRuns (9, 16)
  measurementsScaleMtx <- doBenchmarks scaleMtx backend noRuns (5, 11)
  _ <- plotMeasurements "Scale" [ ("Symbolic", "blue", measurementsScaleSym)
                                , ("Matrix"  , "red" , measurementsScaleMtx)
                                ]

  {-
  measurementsLMapSym <- doBenchmarks lmapSym backend noRuns (9, 16)
  measurementsLMapMtx <- doBenchmarks lmapMtx backend noRuns (5, 11)
  _ <- plotMeasurements "LMap" [ ("Symbolic", "blue", measurementsLMapSym)
                               , ("Matrix"  , "red" , measurementsLMapMtx)
                               ]

  -}



  {-
  genBenchmarks "LMap"  lmapB  C oom noRuns >>= savePlot
  genBenchmarks "Zip"   zipB   C oom noRuns >>= savePlot
  genBenchmarks "Reduce" reduceB C oom noRuns >>= savePlot
  -}
  return ()





{- Compound benchmarks
 - [POPL, p. 8]
 - CNNs RNNs
 - Run on GPU
 -
 - genNN layers  inputsize
 -}

