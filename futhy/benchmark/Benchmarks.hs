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
import Test.Tasty.Bench
import Tests hiding (main)
import Random
import Types hiding (runs)
import Utils
import Flow
import Executer
import Plot (savePlot)
import Json (json2series)

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
scaleB :: Bench
scaleB name backend vecLen runs = benchmark name backend runs (Scale 7.0) (rndVecVals vecLen)

lmapB :: Bench
lmapB name backend vecLen runs  = benchmark name backend runs (LMap (Scale 11.0)) (rndVecVals vecLen)

zipB :: Bench
zipB name backend vecLen runs   = benchmark name backend runs (Zip [Scale 17.0]) (Tensor [rndVecVals vecLen])

reduceB :: Bench
reduceB name backend vecLen runs =
  let relLen = 100 -- (20 *) . floor . log <| (fromIntegral vecLen :: Double)
      maxIdx = vecLen
      maxVal = 100
   in benchmark name backend runs (Red <| rndRelCap relLen maxIdx maxVal) (rndVecVals vecLen)

genBenchmarks :: String -> Bench -> Backend -> Int -> Runs -> IO PlotData
genBenchmarks name bench backend oom runs = do
  let vecLens = powersof2 oom
  cexs <- mapM (\i -> bench (name ++ "_i=" ++ show i) backend i runs) vecLens
  let jsons = map (json . getLog) $ rights cexs
  seriess <- mapM json2series jsons
  return (name, vecLens, seriess)

nnB :: Int -> Bench
nnB numLayers name backend vecLen runs =
  benchmark name backend runs (makeLayersAndErrorFunction numLayers vecLen) (makeNNInput vecLen)

repeatToList f n = map (\_ -> f) [1..n]

getDoubleFromInt :: Int -> Double
getDoubleFromInt = fromIntegral

genRandomUnsafeScalar :: Val
--genRandomUnsafeScalar = Scalar $ getDoubleFromInt $ unsafePerformIO randomIO
genRandomUnsafeScalar = Scalar 2

genVector :: Int -> Val
genVector n = Tensor $ repeatToList genRandomUnsafeScalar n

genSqrMtx :: Int -> Val
genSqrMtx n = Tensor $ repeatToList (genVector n) n

makeNNInput :: Int -> Val
makeNNInput i = let dw = genSqrMtx i
                    dx = genVector i
                    db = genVector i
                in Pair db (Pair dx dw)

makeLayersAndErrorFunction :: Int -> Int -> LFun
makeLayersAndErrorFunction numLayers i = (makeErrorFunction i) `Comp` (makeLayers numLayers i)

makeErrorFunction :: Int -> LFun
makeErrorFunction i = LSec (genVector i) ErrorFunction

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

transposeVal :: Val -> Val
transposeVal x = error "transposeVal"

transposeLFun :: LFun -> LFun
transposeLFun (f `Comp` g)        = (transposeLFun g) `Comp` (transposeLFun f)
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
transposeLFun _ = error "cant transpose this"

main :: IO ()
main = do
--  genBenchmarks "Scale"   scaleB C 16 3 >>= savePlot
--  genBenchmarks "LMap"    lmapB  C 16 3 >>= savePlot
--  genBenchmarks "Zip"     zipB   C 16 3 >>= savePlot
    genBenchmarks "1 NNLayer" (nnB 1) C 8 3 >>= savePlot
    genBenchmarks "2 NNLayer" (nnB 2) C 8 3 >>= savePlot
    genBenchmarks "4 NNLayer" (nnB 4) C 8 3 >>= savePlot
  --genBenchmarks "Reduce" reduceB 16 >>= savePlot
