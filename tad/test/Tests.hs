module Tests (main, runAllTests, caramelizeTestParams, showCleanError) where

--import qualified Prelude
import Prelude hiding (not)
--import qualified Numeric.LinearAlgebra ((<>))
import Control.Monad (when)
import Test.Tasty.HUnit
import Test.Tasty
import Flow

--import Test.QuickCheck
--import GHC.IO.Unsafe
--import Prelude

-- our libs
--import Optimizer
import Interpreter
import Types
import CodeGen
import Utils
import Preprocesser
import Executer-- hiding (main)

  -- separate test files (called from here)
import MatrixTests
import ReduceTests

second :: Integer
second = 1000000

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ second * 30) runAllTests

goodCaseInterpretor :: (LFun, Val, Val) -> TestTree
goodCaseInterpretor params = let (lf, vin, vout) = caramelizeTestParams params in
  testCase "Interpretor" $ interpret lf vin @?= return vout

showCleanError :: Failure -> IO a
showCleanError (CommandFailure _ (_, _, i)) = assertFailure <| remove "\ESC" i --TODO: IMPORTANT: Format the string in output!

genCodeGenrTestCase :: String -> (LFun -> Arity -> Program) -> (LFun, Val, Val) -> TestTree
genCodeGenrTestCase testname codeGenr (lf, vin, vout) =
    testCase testname $ do codeGenRes <- runStrArg (codeGenr lf (getArity vin)) C (stdinShow vin)
                           codeGenResStr <- case codeGenRes of
                                              Right l -> return . stdout . getLog <| l
                                              Left e -> showCleanError e
                           intComp <- runStr ("entry main = " <> show vout) C
                           interpResStrn <- case intComp of
                                              Right l -> return . stdout . getLog <| l
                                              Left e -> showCleanError e
                           when (codeGenResStr /= interpResStrn)
                             <| assertFailure
                             <|  "expected: " ++ show interpResStrn ++ "\n but got: " ++ show codeGenResStr
caramelizeTestParams :: (LFun, Val, Val) -> (LFun, Val, Val)
caramelizeTestParams (lf, vin, vout) = (caramelizeLFun lf, caramelizeVal vin, caramelizeVal vout)

goodCaseExecution :: (LFun, Val, Val) -> TestTree
goodCaseExecution params = genCodeGenrTestCase "CodeGen" codeGenProgram $ caramelizeTestParams params

goodCaseConstExtracted :: (LFun, Val, Val) -> TestTree
goodCaseConstExtracted params =
      let (lf_t, vin_t, vout_t) = caramelizeTestParams params
      in genCodeGenrTestCase "ConstExtracted" codeGenProgramConstExtracted (lf_t, vin_t, vout_t)

goodCaseStaged :: TestName -> (LFun, Val, Val) -> TestTree
goodCaseStaged name params = testGroup name [goodCaseInterpretor params, goodCaseExecution params, goodCaseConstExtracted params]



runAllTests :: TestTree
runAllTests = testGroup "All features" <| concat
  [ [genStdBasisTests]
  , (map testFeature allFeatures)
  , [matrixTests]
  , [optimizerTests]
  ]

testFeature :: (String, [(String, LFun, Val, Val)]) -> TestTree
testFeature (n,l) = testGroup n $ map (\(name, lf, vin, vout) -> goodCaseStaged name (lf, vin, vout)) l

allFeatures :: [(String, [(String, LFun, Val, Val)])]
allFeatures = wipFeatures <> doneFeatures

wipFeatures :: [(String, [(String, LFun, Val, Val)])]
wipFeatures = [ ("reduceTests", reduceTests)
              , ("zipTests", zipTests)
              , ("lmapTests", lmapTests)
              ]

doneFeatures :: [(String, [(String, LFun, Val, Val)])]
doneFeatures = [ ("basic", basicTests)
               , ("addTests", addTests)
               , ("tupleTests", tupleTests)
               , ("miscTests", miscTests)
               , ("dotprod", dotprodTests)
               , ("vecmatmul", vecmatTests)
               , ("matvecmul", matvecTests)
               , ("matmul", matmulTests)
               , ("outer products", outerTests)
               , ("scaleTests", scaleTests)
               , ("lplusTests", lplusTests)
               , ("negTests", negTests)
               ]

tupleTests :: [([Char], LFun, Val, Val)]
tupleTests =
  [ ("Comp Add (Para Add Id)"
        , Comp Add (Para Add Id)
        , Pair (Pair (Scalar 1.0) (Scalar 2.0)) (Scalar 3.0)
        , Scalar 6.0)
    , ("Id (P (P (P S S) S) (P S S))"
        , Id
        , Pair (Pair (Pair (Scalar 1.0) (Scalar 2.0))(Scalar 2.0)) (Pair (Scalar 1.0) (Scalar 2.0))
        , Pair (Pair (Pair (Scalar 1.0) (Scalar 2.0))(Scalar 2.0)) (Pair (Scalar 1.0) (Scalar 2.0)))
  ]

basicTests :: [([Char], LFun, Val, Val)]
basicTests =
  [ ("Id scalar"
        , Id
        , Scalar 15.0
        , Scalar 15.0)
  , ("Dupe"
        , Dup
        , Scalar 1.0
        , Pair (Scalar 1.0) (Scalar 1.0))
  , ("Id vector"
        , Id
        , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
        , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0])
  , ("Comp id and scale"
        , Comp Id (Scale 7.0)
        , Scalar 10.0
        , Scalar 70.0)
  , ("Comp scale scale"
        , Comp  (Scale (-2.0)) (Scale 7.0)
        , Scalar 10.0
        , Scalar (-140.0))
  , ("Comp scale scale scale on vector"
        , Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0)
        , Vector [Scalar 1.0, Scalar 2.0]
        , Vector [Scalar 168.0, Scalar 336.0])
  , ("Para"
        , Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup
        , Scalar 1.0
        , Pair (Scalar 3.0) (Scalar 7.0))
  , ("Lsec scalar * scalar"
        , LSec (Scalar 2.0) Outer
        , Scalar 2.0
        , Scalar 4.0)
  , ("Lsec scalar * scalar"
        , LSec (Scalar 2.0) Outer
        , Scalar 2.0
        , Scalar 4.0)
  , ("Id ^+ K0 $ 6.0 -> 6.0"
        , LPlus Id KZero
        , Scalar 6.0
        , Scalar 6.0)
  , ("(Scale (-3.0)) ^+ (Scale 5.0) 7.0 -> 14.0"
        , LPlus (Scale (-3.0)) (Scale 5.0)
        , Scalar 7.0
        , Scalar 14.0)
  ]

miscTests :: [([Char], LFun, Val, Val)]
miscTests =
  [ ("Id (+) K0 $ (3.0, 5.0) -> (3.0, 0.0)"
        , Para Id KZero
        , Pair (Scalar 3.0) (Scalar 5.0)
        , Pair (Scalar 3.0) (Scalar 0.0))
  ]

scaleTests :: [([Char], LFun, Val, Val)]
scaleTests =
  [ ("Scale scalar"
        , Scale 3.0
        , Scalar 5.0
        , Scalar 15.0)
  , ("Scale vector"
        , Scale 3.0
        , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
        , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0])
  , ("Scale matrix"
        , Scale 3.0
        , Vector [ Vector [Scalar 1.0, Scalar 2.0]
                 , Vector [Scalar 3.0, Scalar 4.0]]
        , Vector [ Vector [Scalar 3.0, Scalar 6.0]
                 , Vector [Scalar 9.0, Scalar 12.0]])
  , ("Scale R3 Vector"
        , Scale 3.0
        , Vector [ Vector [ Vector [Scalar 1.0, Scalar 2.0]
                          , Vector [Scalar 3.0, Scalar 4.0]]
                 , Vector [ Vector [Scalar 5.0, Scalar 6.0]
                          , Vector [Scalar 7.0, Scalar 8.0]]]
        , Vector [ Vector [ Vector [Scalar 3.0, Scalar 6.0]
                          , Vector [Scalar 9.0, Scalar 12.0]]
                 , Vector [ Vector [Scalar 15.0, Scalar 18.0]
                          , Vector [Scalar 21.0, Scalar 24.0]]])
  ]

dotprodTests :: [([Char], LFun, Val, Val)]
dotprodTests =
  [ ("[1,2,3] * [4,5,6] = 32"
        , LSec (Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]) DotProd
        , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]
        , Scalar 32.0)
  ]

vecmatTests :: [([Char], LFun, Val, Val)]
vecmatTests =
  [ ("LSec 3x1 * 3x2 -> 1x2"
          , LSec (mkT1 [1,2,3]) VecMatProd
          , mkT2 [[1,2]
                 ,[3,4]
                 ,[5,6]]
          , Vector [Scalar 22.0, Scalar 28.0])
  , ("RSec 3x1 * 3x2 -> 2x1"
          , RSec VecMatProd (Vector [ Vector [Scalar 1.0, Scalar 2.0]
                                    , Vector [Scalar 3.0, Scalar 4.0]
                                    , Vector [Scalar 5.0, Scalar 6.0]])
          , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
          , Vector [Scalar 22.0, Scalar 28.0])
  ]

matvecTests :: [([Char], LFun, Val, Val)]
matvecTests =
  [ ("LSec 2x3 * 3 -> 2"
          , LSec (Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                         , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]]) MatVecProd
          , Vector [ Scalar 1.0, Scalar 2.0, Scalar 2.0]
          , Vector [ Scalar 11.0, Scalar 26.0])
  , ("RSec 3x2 * 3 -> 2"
          , RSec MatVecProd (Vector [Scalar 1.0, Scalar 2.0, Scalar 2.0])
          , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                   , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]]
          , Vector [Scalar 11.0, Scalar 26.0])
  ]

matmulTests :: [([Char], LFun, Val, Val)]
matmulTests =
  [ ("LSec 2x2 * 2x2 -> 2x2"
          , LSec (Vector [ Vector [Scalar 1.0, Scalar 2.0]
                         , Vector [Scalar 3.0, Scalar 4.0]]) MatrixMult
          , Vector [ Vector [Scalar 5.0, Scalar 6.0]
                   , Vector [Scalar 7.0, Scalar 8.0]]
          , Vector [ Vector [Scalar 19.0, Scalar 22.0]
                   , Vector [Scalar 43.0, Scalar 50.0]])
  , ("LSec 2x3 * 3x2 -> 2x2"
          , LSec (Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                         , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]]) MatrixMult
          , Vector [ Vector [Scalar 7.0, Scalar 8.0]
                   , Vector [Scalar 9.0, Scalar 10.0]
                   , Vector [Scalar 11.0, Scalar 12.0]]
          , Vector [ Vector [Scalar 58.0, Scalar 64.0]
                   , Vector [Scalar 139.0, Scalar 154.0]])
  , ("LSec 3x2 * 2x3 -> 3x3"
          , LSec (Vector [ Vector [Scalar 7.0, Scalar 8.0]
                         , Vector [Scalar 9.0, Scalar 10.0]
                         , Vector [Scalar 11.0, Scalar 12.0]]) MatrixMult
          , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                    , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]]
          , Vector [ Vector [Scalar 39.0, Scalar 54.0, Scalar 69.0]
                   , Vector [Scalar 49.0, Scalar 68.0, Scalar 87.0]
                   , Vector [Scalar 59.0, Scalar 82.0, Scalar 105.0]])


  , ("RSec 2x2 * 2x2 -> 2x2"
          , RSec MatrixMult (Vector [ Vector [Scalar 1.0, Scalar 2.0]
                         , Vector [Scalar 3.0, Scalar 4.0]])
          , Vector [ Vector [Scalar 5.0, Scalar 6.0]
                   , Vector [Scalar 7.0, Scalar 8.0]]
          , Vector [ Vector [Scalar 23.0, Scalar 34.0]
                   , Vector [Scalar 31.0, Scalar 46.0]])
  , ("RSec 2x3 * 3x2 -> 2x2"
          , RSec MatrixMult (Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                         , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]])
          , Vector [ Vector [Scalar 7.0, Scalar 8.0]
                   , Vector [Scalar 9.0, Scalar 10.0]
                   , Vector [Scalar 11.0, Scalar 12.0]]
          , Vector [ Vector [Scalar 39.0, Scalar 54.0, Scalar 69.0]
                   , Vector [Scalar 49.0, Scalar 68.0, Scalar 87.0]
                   , Vector [Scalar 59.0, Scalar 82.0, Scalar 105.0]])
  , ("RSec 3x2 * 2x3 -> 3x3"
          , RSec MatrixMult (Vector [ Vector [Scalar 7.0, Scalar 8.0]
                         , Vector [Scalar 9.0, Scalar 10.0]
                         , Vector [Scalar 11.0, Scalar 12.0]])
          , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                   , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]]
          , Vector [ Vector [Scalar 58.0, Scalar 64.0]
                   , Vector [Scalar 139.0, Scalar 154.0]])
  ]


outerTests :: [([Char], LFun, Val, Val)]
outerTests =
  [ ("1 {outer product} 1 -> 2"
          , LSec (Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer
          , Vector [Scalar 5.0, Scalar 2.0, Scalar 3.0]
          , Vector [ Vector [Scalar 5.0, Scalar 2.0, Scalar 3.0]
                   , Vector [Scalar 10.0, Scalar 4.0, Scalar 6.0]
                   , Vector [Scalar 15.0, Scalar 6.0, Scalar 9.0]])
  ,  ("2 {outer product} 1 -> 2"
          , LSec (Vector [Scalar 1.0, Scalar 2.0]) Outer
          , Scalar 3.0
          , Vector [Scalar 3.0, Scalar 6.0])
  , ("1 {outer product} 2 -> 2"
          , LSec (Scalar 2.0) Outer
          , Vector [Scalar 2.0, Scalar 3.0]
          , Vector [Scalar 4.0, Scalar 6.0])
  , ("2 {outer product} 2 -> 2x3"
          , LSec (Vector [Scalar 2.0, Scalar 3.0]) Outer
          , Vector [Scalar 4.0, Scalar 5.0]
          , Vector [ Vector [Scalar 8.0, Scalar 10.0]
                   , Vector [Scalar 12.0, Scalar 15.0]])
  , ("2 {outer product} 3 -> 2x3"
          , LSec (Vector [Scalar 2.0, Scalar 3.0]) Outer
          , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]
          , Vector [ Vector [Scalar 8.0, Scalar 10.0, Scalar 12.0]
                   , Vector [Scalar 12.0, Scalar 15.0, Scalar 18.0]])
  , ("3 {outer product} 2 -> 3x2"
          , RSec Outer (Vector [Scalar 2.0, Scalar 3.0])
          , Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]
          , Vector [ Vector [Scalar 8.0, Scalar 12.0]
                   , Vector [Scalar 10.0, Scalar 15.0]
                   , Vector [Scalar 12.0, Scalar 18.0]])
 , ("3 * 3x3 -> 3x3x3"
          , LSec (Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer
          , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                   , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                   , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]]
          , Vector [ Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                            , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                            , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]]
                   , Vector [ Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                            , Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                            , Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]]
                   , Vector [ Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]
                            , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]
                            , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]]])
  , ("3x3 * 3 -> 3x3x3"
          , LSec (Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                         , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                         , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]]) Outer
          , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
          , Vector [ Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                            , Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                            , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]]
                   , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                            , Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                            , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]]
                   , Vector [ Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                            , Vector [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                            , Vector [Scalar 3.0, Scalar 6.0, Scalar 9.0]]])
  ]

lplusTests :: [([Char], LFun, Val, Val)]
lplusTests =
  [ ("lplus_0"
        , LPlus  (Scale 3.0) (Scale 7.0)
        , Scalar 1.0
        , Scalar 10.0)
  , ("lplus_1"
        , LPlus  (Scale 3.0) (Scale 7.0)
        , Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]
        , Vector [Scalar 10.0, Scalar 20.0, Scalar 30.0])
  ]

negTests :: [([Char], LFun, Val, Val)]
negTests =
  [ ("Neg scalar"
        , Neg
        , Scalar 5.0
        , Scalar (-5.0))
  , ("Neg minus scalar"
        , Neg
        , Scalar (-5.0)
        , Scalar 5.0)
  , ("Neg dense Vector"
        , Neg
        , Vector [Vector [Scalar 8.0, Scalar 12.0], Vector [Scalar 10.0, Scalar 15.0], Vector [Scalar 12.0, Scalar 18.0]]
        , Vector [Vector [Scalar (-8.0), Scalar (-12.0)], Vector [Scalar (-10.0), Scalar (-15.0)], Vector [Scalar (-12.0), Scalar (-18.0)]])
  ]

lmapTests :: [([Char], LFun, Val, Val)]
lmapTests =
  [ ("LMap: map to zero"
          , LMap KZero
          , Vector [Scalar (-1), Scalar 1, Scalar 2, Scalar 3]
          , Vector [Scalar (-0), Scalar 0, Scalar 0, Scalar 0])
  ]

zipTests :: [([Char], LFun, Val, Val)]
zipTests =
  [ ("Zip: simple Scales"
          , Zip [Scale 2, Scale 3, Scale 4, Scale 6]
          , Vector [Scalar (-1), Scalar 1,Scalar 2,Scalar 3]
          , Vector [Scalar (-2), Scalar 3,Scalar 8, Scalar 18])
  , ("Zip: dot products"
          , Zip [LSec (Vector [Scalar 1.0, Scalar 2.0, Scalar 3.0]) DotProd, LSec (Vector [Scalar 4.0, Scalar 5.0, Scalar 6.0]) DotProd, LSec (Vector [Scalar 7.0, Scalar 8.0, Scalar 9.0]) DotProd]
          , Vector [Vector [Scalar 10, Scalar 11, Scalar 12], Vector [Scalar 13, Scalar 14, Scalar 15], Vector [Scalar 16, Scalar 17, Scalar 18]]
          , Vector [Scalar 68, Scalar 212, Scalar 410])
  , ("Zip: dup"
          , Zip [Dup, Dup, Dup]
          , Vector [Scalar (-1), Scalar 1,Scalar 3]
          , Vector [Pair (Scalar (-1)) (Scalar (-1)), Pair (Scalar 1) (Scalar 1),Pair (Scalar 3) (Scalar 3)])
  , ("Zip: comp add dup"
          , Zip [Comp Add Dup, Comp Add Dup, Comp Add Dup]
          , Vector [Scalar (-1), Scalar 1,Scalar 3]
          , Vector [Scalar (-2), Scalar 2, Scalar 6])
  , ("Zip: comp dup scale"
          , Zip [Comp Dup (Scale 2), Comp Dup (Scale 2), Comp Dup (Scale 2)]
          , Vector [Scalar (-1), Scalar 1,Scalar 3]
          , Vector [Pair (Scalar (-2)) (Scalar (-2)), Pair (Scalar 2) (Scalar 2),Pair (Scalar 6) (Scalar 6)])
  , ("Para: Zip scale Zip scale"
          , Para (Zip [Scale 1, Scale 3,Scale 3]) (Zip [Scale 2,Scale 4, Scale 4])
          , Pair (Vector [Scalar (-1), Scalar 5, Scalar 7]) (Vector [Scalar (-2), Scalar 6, Scalar 8])
          , Pair (Vector [Scalar (-1), Scalar 15, Scalar 21]) (Vector [Scalar (-4), Scalar 24, Scalar 32]))
  ]

addTests :: [(String, LFun, Val, Val)]
addTests =
  [ ("Add: left Zero and scalar"
    ,  Add
    ,  Pair Zero (Scalar 5)
    ,  Scalar 5
    )
  , ("Add: scalar right Zero"
    ,  Add
    ,  Pair (Scalar 3) Zero
    ,  Scalar 3
    )
  , ("Add: scalar and scalar"
    ,  Add
    ,  Pair (Scalar 3) (Scalar 5)
    ,  Scalar 8
    )
  , ("Add: dense Vector and dense Vector"
    ,  Add
    ,  Pair (Vector [Scalar 2, Scalar 3, Scalar 5]) (Vector [Scalar 13, Scalar 7, Scalar 11])
    ,  Vector [Scalar 15, Scalar 10, Scalar 16]
    )
  ]




--instance Arbitrary Val where
--  arbitrary = oneof
--    [ pure $ Scalar 1.0 ]

--instance Arbitrary LFun where
--  arbitrary = oneof
--    [ pure Id ]

--propInterpretorCodeGenrEqual :: LFun -> Val -> Property
--propInterpretorCodeGenrEqual lf vin =

--qcTests :: TestTree
--qcTests =
--  testGroup "qc tests"
--    [ testCase "propInterpretorCodeGenrEqual"
--      $ quickCheckWith stdArgs { maxSuccess = 1 } propInterpretorCodeGenrEqual
--    ]

goodCaseOptimizer :: TestName -> LFun -> LFun -> TestTree
goodCaseOptimizer name vin vout = testCase name $ optimize vin @?= vout

optimizerTests :: TestTree
optimizerTests =
  testGroup "Optimizer"
    [ goodCaseOptimizer "Comp Id Id -> Id"
      (Comp (Comp Id (Comp Id Id)) (Comp Id Id))
      (Id)
    , goodCaseOptimizer "Combining many scales"
      (Comp (Comp (Scale 2.0) (Comp (Scale 2.0) (Comp (Comp (Scale 2.0) (Comp (Scale 2.0) (Scale 2.0))) (Comp (Scale 2.0) (Scale 2.0))))) (Comp (Scale 2.0) (Scale 2.0)))
      (Scale 512.0)
   , goodCaseOptimizer "Combining many paras"
      (Comp (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0)))) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0)))))) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0))))
      (Para (Scale 512.0) (Scale 512.0))
    , goodCaseOptimizer "Flattening"
      (Comp (Comp (KZero) (Comp (KZero) (Comp (Comp (KZero) (Comp (KZero) (KZero))) (Comp (KZero) (KZero))))) (Comp (KZero) (KZero)))
      (Comp KZero (Comp KZero (Comp KZero (Comp KZero (Comp KZero (Comp KZero (Comp KZero (Comp KZero KZero))))))))
    ]
