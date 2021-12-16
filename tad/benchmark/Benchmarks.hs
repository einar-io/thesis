module Benchmarks (main) where

{-
1. Measure time for interpreter on random value.
2. Make benchmarks for wipFeatures to run on the interpreter benchmarks.
3. Generate random values on command.
4. Expand to compiler.
-}

import Tests (showCleanError)
import System.Random
import System.IO.Unsafe
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


{- New-flavour benchmarks for testing GPU -}
scaleSym :: Bench
scaleSym backend runs inputLen =
  let lfn = Scale 7.0
      pgmfile = "scaleSym_" ++ show inputLen ++ ".fut"
      dataset = "dataset_"  ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs lfn (rndVecVals inputLen)

scaleMtx :: Bench
scaleMtx backend runs inputLen =
  let lfn = Scale 59.0
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
      pgmfile = "scaleMtx_" ++ show inputLen ++ ".fut"
      dataset = "dataset_"  ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs mlfn (rndVecVals inputLen)


lmapSym :: Bench
lmapSym backend runs inputLen =
  let lfn = LMap (Scale 11.0)
      pgmfile = "lmapSym_" ++ show inputLen ++ ".fut"
      dataset = "dataset_" ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs lfn (rndVecVals inputLen)

lmapMtx :: Bench
lmapMtx backend runs inputLen =
  let lfn = Scale 59.0
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
      pgmfile = "lmapMtx_" ++ show inputLen ++ ".fut"
      dataset = "dataset_" ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs mlfn (rndVecVals inputLen)


zipSym :: Bench
zipSym backend runs inputLen =
  let lfn = Zip (replicate inputLen (Scale 17.0))
      pgmfile = "zipSym_"  ++ show inputLen ++ ".fut"
      dataset = "dataset_" ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs lfn (rndVecVals inputLen)

zipMtx :: Bench
zipMtx backend runs inputLen =
  let lfn = Zip (replicate inputLen (Scale 17.0))
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
      pgmfile = "zipMtx_"  ++ show inputLen ++ ".fut"
      dataset = "dataset_" ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs mlfn (rndVecVals inputLen)

{-
<zfnmxt> That coupled with the fact that your relation length is logarithmic
with the size of your input means that the amount of work to do is nearly
constant regardless of the input size

<zfnmxt> As a rule, I'd make my input, output, and relation size all about the
same (up] to a few smallish constant factors). Then you should see runtimes
scale better.
-}

redSym :: Bench
redSym backend runs inputLen =
  let relLen = inputLen
      maxIdx = inputLen
      maxVal = 100
      lfn = Red <| rndRelCap relLen maxIdx maxVal
      pgmfile = "redSym_"  ++ show inputLen ++ ".fut"
      dataset = "dataset_" ++ show inputLen ++ ".val"
   in benchmark pgmfile dataset backend runs lfn (rndVecVals inputLen)

{-
redMtx :: Bench
redMtx backend runs inputLen =
  let relLen = inputLen
      maxIdx = inputLen
      maxVal = 100
      lfn = Red <| rndRelCap relLen maxIdx maxVal
      mtx = getMatrixRep lfn [inputLen]
      mlfn = LSec mtx MatrixMult
   in benchmark "redMtx" inputLen backend runs mlfn (rndVecVals inputLen)

-}

doBenchmarks :: Bench -> Backend -> Runs -> OOMs -> IO ([Int], [Double])
doBenchmarks bench backend runs ooms = do
  let inputLens = powersof2 ooms
  cexs <- mapM (bench backend runs) inputLens
  print ( "LENGHT: "  ++ (show . length $ cexs)
    ++ "; noRights: " ++ (show . length . rights $ cexs)
    ++ "; noLefts: "  ++ (show . length . lefts $ cexs ))
  print (lefts cexs)
  guard (length (rights cexs) == length inputLens)

  let jsons = map (json . getLog) (rights cexs)
  seriess <- mapM json2series jsons -- we don't need to do this in IO
  let series = map minimum seriess -- this is important
  return (inputLens, series)


------------------ Neural Network Examples

nn :: Int -> Bench
nn numLayers backend runs inputLen =
  let lfn = makeLayersAndLossFunction numLayers inputLen
      dataset = "dataset_nn_" ++ show inputLen ++ ".val"
      pgmfile = "nn_" ++ show numLayers ++ "l_" ++ show inputLen ++ ".fut"
   in benchmark pgmfile dataset backend runs lfn (makeNNForwardModeInput inputLen)


{-
nnBOld :: Int -> Bench
nnBOld numLayers name backend vecLen runs =
  benchmark name backend runs (makeLayersAndLossFunction numLayers vecLen) (makeNNInput vecLen)
-}

repeatToList :: (Num a, Enum a) => b -> a -> [b]
repeatToList f n = map (\_ -> f) [1..n]

getDoubleFromInt :: Int -> Double
getDoubleFromInt = fromIntegral

genRandomUnsafeScalar :: Val
--genRandomUnsafeScalar = Scalar $ getDoubleFromInt $ unsafePerformIO randomIO
genRandomUnsafeScalar = Scalar 2

genVector :: Int -> Val
genVector n = Vector $ repeatToList genRandomUnsafeScalar n

genSqrMtx :: Int -> Val
genSqrMtx n = Vector $ repeatToList (genVector n) n

makeNNForwardModeInput :: Int -> Val
makeNNForwardModeInput i =
  let dw = genSqrMtx i
      dx = genVector i
      db = genVector i
   in Pair db (Pair dx dw)

makeLayersAndLossFunction :: Int -> Int -> LFun
makeLayersAndLossFunction numLayers i = (LSec (genVector i) LossFunction) `Comp` (makeLayers numLayers i)

makeLayers :: Int -> Int -> LFun
makeLayers 1 i = makeLayer i
makeLayers n i = (makeLayer i) `Comp` (makeLayers (n-1) i)

makeLayer :: Int -> LFun
makeLayer i = let w = genSqrMtx i
                  x = genVector i
                  --secondPart = ((((LSec w MatrixMult) `Comp` (Snd `Comp` Fst)) `Lplus` ( `Comp` (Fst `Comp` Fst))) `Lplus` Snd)
                  --secondPart = (RSec MatrixMult w)
               in Zip (map (\(Scalar a) -> Scale $ 1 - ((tanh a)^2)) $ map fromIntegral [1..i])
                 `Comp`
                  Add `Comp` (Para Id (Add `Comp` ((LSec w VecMatProd) `Para` (RSec MatVecProd x))))

{- [t3, p. 6, Adjunction rules] -}
transposeLFun :: LFun -> LFun
transposeLFun (f `Comp` g)        = (transposeLFun g) `Comp` (transposeLFun f)
transposeLFun Id                  = Id
transposeLFun KZero               = KZero
transposeLFun (LSec x MatrixMult) = LSec (transposeVal x) MatrixMult
transposeLFun (RSec MatrixMult x) = RSec MatrixMult (transposeVal x)
transposeLFun Fst                 = InjFst
transposeLFun Snd                 = InjSnd
transposeLFun (f `Lplus` g)       = (transposeLFun g) `Lplus` (transposeLFun f)
transposeLFun (LSec x DotProd)    = LSec x DotProd
transposeLFun (RSec DotProd x)    = RSec DotProd x
transposeLFun Neg                 = Neg
transposeLFun (Scale x)           = Scale x
transposeLFun (Zip ls)            = Zip $ map transposeLFun ls
transposeLFun (LMap f)            = LMap (transposeLFun f)
transposeLFun other               = error <| "Transposition of `" ++ show other ++ "` not implemented."

main :: IO ()
main = do

  let backend = OPENCL
  let oom = (1, 16) -- ordersOfMagnitude of 2 of the datasets.  Should be more than 10
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
  nn1LayerMeas <- doBenchmarks (nn 1) backend noRuns (5, 11)
  nn2LayerMeas <- doBenchmarks "2 NNLayer" (nnB 2) backend noRuns (2, 8)
  nn4LayerMeas <- doBenchmarks "4 NNLayer" (nnB 4) backend noRuns (2, 8)
  _ <- plotMeasurements "Neural Networks"
    [ ("1-Layer Neural Network", "blue", nn1LayerMeas)
   {- , ("2-Layer Neural Network", "red", nn2LayerMeas)
    , ("4-Layer Neural Network", "green", nn4LayerMeas)
    -}
    ]
  -}


  {-
  genBenchmarks "1 NNLayer" (nnB 1) C 8 3 >>= savePlot
  genBenchmarks "2 NNLayer" (nnB 2) C 8 3 >>= savePlot
  genBenchmarks "4 NNLayer" (nnB 4) C 8 3 >>= savePlot
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
