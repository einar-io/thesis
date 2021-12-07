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
  let lfn = Scale 59.0
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
   in benchmark "ScaleMtx" inputLen backend runs mlfn (rndVecVals inputLen)


lmapSym :: Bench
lmapSym backend runs inputLen =
  let lfn = LMap (Scale 11.0)
   in benchmark "LmapSym" inputLen backend runs lfn (rndVecVals inputLen)

lmapMtx :: Bench
lmapMtx backend runs inputLen =
  let lfn = Scale 59.0
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
   in benchmark "LmapMtx" inputLen backend runs mlfn (rndVecVals inputLen)


zipSym :: Bench
zipSym backend runs inputLen =
  let lfn = Zip (replicate inputLen (Scale 17.0))
   in benchmark "ZipSym" inputLen backend runs lfn (rndVecVals inputLen)

{-
zipMtx :: Bench
zipMtx backend runs inputLen =
  let lfn = Zip (replicate inputLen (Scale 17.0))
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
   in benchmark "zipMtx" inputLen backend runs mlfn (rndVecVals inputLen)
-}

zipMtx :: Bench
zipMtx backend runs inputLen =
  let lfn = Zip (replicate inputLen (Scale 17.0))
            |> flip getMatrixRep [inputLen]
            |> flip LSec MatrixMult
   in benchmark "zipMtx" inputLen backend runs lfn (rndVecVals inputLen)

{-
<zfnmxt> That coupled with the fact that your relation length is logarithmic with the size of your input means that the amount of work to do is nearly constant regardless of the input size
<zfnmxt> As a rule, I'd make my input, output, and relation size all about the same (up]
 to a few smallish constant factors). Then you should see runtimes scale better.
-}
redSym :: Bench
redSym backend runs inputLen =
  let relLen = inputLen
      maxIdx = inputLen
      maxVal = 100
      lfn = Red <| rndRelCap relLen maxIdx maxVal
   in benchmark "redSym" inputLen backend runs lfn (rndVecVals inputLen)

redMtx :: Bench
redMtx backend runs inputLen =
  let relLen = inputLen
      maxIdx = inputLen
      maxVal = 100
      lfn = Red <| rndRelCap relLen maxIdx maxVal
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
   in benchmark "redMtx" inputLen backend runs mlfn (rndVecVals inputLen)

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

  scaleSymMeas <- doBenchmarks scaleSym backend noRuns (9, 16)
  scaleMtxMeas <- doBenchmarks scaleMtx backend noRuns (5, 11)
  _ <- plotMeasurements "Scale" [ ("Symbolic", "blue", scaleSymMeas)
                                , ("Matrix"  , "red" , scaleMtxMeas)
                                ]

  lMapSymMeas <- doBenchmarks lmapSym backend noRuns (9, 16)
  lMapMtxMeas <- doBenchmarks lmapMtx backend noRuns (5, 11)
  _ <- plotMeasurements "LMap" [ ("Symbolic", "blue", lMapSymMeas)
                               , ("Matrix"  , "red" , lMapMtxMeas)
                               ]


  zipSymMeas <- doBenchmarks zipSym backend noRuns (8, 14)
  zipMtxMeas <- doBenchmarks zipMtx backend noRuns (5, 10)
  _ <- plotMeasurements "Zip" [ ("Symbolic", "blue", zipSymMeas)
                              , ("Matrix"  , "red" , zipMtxMeas)
                              ]


  redSymMeas <- doBenchmarks redSym backend noRuns (8, 14)
  --redMtxMeas <- doBenchmarks redMtx backend noRuns (5, 5)
  _ <- plotMeasurements "Red" [ ("Symbolic", "blue", redSymMeas)
                              --, ("Matrix"  , "red" , redMtxMeas)
                              ]

  {-
  genBenchmarks "Reduce" reduceB C oom noRuns >>= savePlot

  genBenchmarks "NN" reduceB C oom noRuns >>= savePlot
  -}
  return ()





{- Compound benchmarks
 - [POPL, p. 8]
 - CNNs RNNs
 - Run on GPU
 -
 - genNN layers  inputsize
 -}

