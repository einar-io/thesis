import Test.Tasty.HUnit
import Test.Tasty
--import Test.QuickCheck
--import GHC.IO.Unsafe
import Prelude

-- our libs
import Optimizer
import Interpretor
import Types
import Compiler
import Utils
import Executor hiding (main)

second :: Integer
second = 1000000

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ second * 30) runAllTests

goodCaseInterpretor :: TestName -> LFun -> Val -> Val -> TestTree
goodCaseInterpretor name lfun vin vout = testCase name $ interpret lfun vin @?= return vout

goodCaseExecution :: TestName -> LFun -> Val -> TestTree
goodCaseExecution name lf vin =
  testCase name $ do compileRes <- runStrArg (compileProgram lf (getArity vin)) C (show vin)
                     compileResStr  <- case compileRes of
                                        Right (Output (_, res, _)) -> return res
                                        e -> assertFailure $ show e
                     let Right intVal = interpret lf vin
                     intComp <- runStr ("entry main = " <> show intVal) C
                     interpResStrn <-  case intComp of
                                Right (Output (_, ref, _)) -> return ref
                                e -> assertFailure $ show e
                     case (compileResStr == interpResStrn) of
                      False -> assertFailure $ show (compileResStr, interpResStrn)
                      True -> return ()

goodCaseStaged :: TestName -> LFun -> Val -> Val -> TestTree
goodCaseStaged name lf vin vout = testGroup name $ [goodCaseInterpretor "Interpretor" lf vin vout, goodCaseExecution "Compiler" lf vin]

runAllTests :: TestTree
runAllTests = testGroup "All features" $ [optimizerTests] <> map testFeature allFeatures

testFeature :: (String, [(String, LFun, Val, Val)]) -> TestTree
testFeature (n,l) = testGroup n $ map (\(name, lf, vin, vout) -> goodCaseStaged name lf vin vout) l

allFeatures :: [(String, [(String, LFun, Val, Val)])]
allFeatures = [ ("basic", basicTests)
              , ("outer products", outerTests)
              , ("unsorted", otherTests)
              ]

basicTests :: [(String, LFun, Val, Val)]
basicTests = [ ("Id scalar"
                    , Id
                    , Scalar 15.0
                    , Scalar 15.0)
             , ("Dupe"
                    , Dup
                    , Scalar 1.0
                    , Pair (Scalar 1.0) (Scalar 1.0))
             , ("Id vector"
                    , Id
                    , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                    , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
             ]

outerTests :: [(String, LFun, Val, Val)]
outerTests = [ ("2 {outer product} 1 -> 2"
                    , LSec (Tensor [Scalar 1.0, Scalar 2.0]) Outer
                    , Scalar 3.0
                    , Tensor [Scalar 3.0, Scalar 6.0])
             , ("1 {outer product} 2 -> 2"
                    , LSec (Scalar 2.0) Outer
                    , Tensor [Scalar 2.0, Scalar 3.0]
                    , Tensor [Scalar 4.0, Scalar 6.0])
             , ("2 {outer product} 2 -> 2x3"
                    , LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer
                    , Tensor [Scalar 4.0, Scalar 5.0]
                    , Tensor [ Tensor [Scalar 8.0, Scalar 10.0]
                             , Tensor [Scalar 12.0, Scalar 15.0]])
             , ("2 {outer product} 3 -> 2x3"
                    , LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer
                    , Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0]
                    , Tensor [ Tensor [Scalar 8.0, Scalar 10.0, Scalar 12.0]
                             , Tensor [Scalar 12.0, Scalar 15.0, Scalar 18.0]])
             , ("3 {outer product} 2 -> 3x2"
                    , RSec Outer (Tensor [Scalar 2.0, Scalar 3.0])
                    , Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0]
                    , Tensor [ Tensor [Scalar 8.0, Scalar 12.0]
                             , Tensor [Scalar 10.0, Scalar 15.0]
                             , Tensor [Scalar 12.0, Scalar 18.0]])
             , ("3 * 3x3 -> 3x3x3"
                    , LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer
                    , Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                             , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                             , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]]
                    , Tensor [ Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                      , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                      , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]]
                             , Tensor [ Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                                      , Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                                      , Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]]
                             , Tensor [ Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]
                                      , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]
                                      , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]]])
             , ("3x3 * 3 -> 3x3x3"
                    , LSec (Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                   , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                   , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]]) Outer
                    , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                    , Tensor [ Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                      , Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                                      , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]]
                             , Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                      , Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                                      , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]]
                             , Tensor [ Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                                      , Tensor [Scalar 2.0, Scalar 4.0, Scalar 6.0]
                                      , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0]]])

             , ("Scale scalar"
                    , Scale 3.0
                    , Scalar 5.0
                    , Scalar 15.0)
             , ("Scale vector"
                    , Scale 3.0
                    , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                    , Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0])
             , ("Scale matrix"
                    , Scale 3.0
                    , Tensor [ Tensor [Scalar 1.0, Scalar 2.0]
                             , Tensor [Scalar 3.0, Scalar 4.0]]
                    , Tensor [ Tensor [Scalar 3.0, Scalar 6.0]
                             , Tensor [Scalar 9.0, Scalar 12.0]])
             , ("Scale R3 tensor"
                    , Scale 3.0
                    , Tensor [ Tensor [ Tensor [Scalar 1.0, Scalar 2.0]
                                      , Tensor [Scalar 3.0, Scalar 4.0]]
                             , Tensor [ Tensor [Scalar 5.0, Scalar 6.0]
                                      , Tensor [Scalar 7.0, Scalar 8.0]]]
                    , Tensor [ Tensor [ Tensor [Scalar 3.0, Scalar 6.0]
                                      , Tensor [Scalar 9.0, Scalar 12.0]]
                             , Tensor [ Tensor [Scalar 15.0, Scalar 18.0]
                                      , Tensor [Scalar 21.0, Scalar 24.0]]])
             ]

otherTests :: [(String, LFun, Val, Val)]
otherTests = [ ("Comp id and scale"
                    , Comp Id (Scale 7.0)
                    , Scalar 10.0
                    , Scalar 70.0)
             , ("Comp scale scale"
                    , Comp  (Scale (-2.0)) (Scale 7.0)
                    , Scalar 10.0
                    , Scalar (-140.0))
             , ("Comp scale scale scale on vector"
                    , Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0)
                    , Tensor [Scalar 1.0, Scalar 2.0]
                    , Tensor [Scalar 168.0, Scalar 336.0])
             , ("Para"
                    , Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup
                    , Scalar 1.0
                    , Pair (Scalar 3.0) (Scalar 7.0))
             , ("lplus_0"
                    , Lplus  (Scale 3.0) (Scale 7.0)
                    , Scalar 1.0
                    , Scalar 10.0)
             , ("lplus_1"
                    , Lplus  (Scale 3.0) (Scale 7.0)
                    , Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]
                    , Tensor [Scalar 10.0, Scalar 20.0, Scalar 30.0])
             , ("Lsec scalar * scalar"
                    , LSec (Scalar 2.0) Outer
                    , Scalar 2.0
                    , Scalar 4.0)
             , ("Lsec scalar * scalar"
                    , LSec (Scalar 2.0) Outer
                    , Scalar 2.0
                    , Scalar 4.0)
             , ("Id (+) K0 (3.0, 5.0) -> (3.0, 0.0)"
                    , Para Id KZero
                    , Pair (Scalar 3.0) (Scalar 5.0)
                    , Pair (Scalar 3.0) (Scalar 0.0))
             , ("Id ^+ K0 6.0 -> 6.0"
                    , Lplus Id KZero
                    , Scalar 6.0
                    , Scalar 6.0)
             , ("(Scale (-3.0)) ^+ (Scale 5.0) 7.0 -> 14.0"
                    , Lplus (Scale (-3.0)) (Scale 5.0)
                    , Scalar 7.0
                    , Scalar 14.0)
             ]

--instance Arbitrary Val where
--  arbitrary = oneof
--    [ pure $ Scalar 1.0 ]

--instance Arbitrary LFun where
--  arbitrary = oneof
--    [ pure Id ]

--propInterpretorCompilerEqual :: LFun -> Val -> Property
--propInterpretorCompilerEqual lf vin =

--qcTests :: TestTree
--qcTests =
--  testGroup "qc tests"
--    [ testCase "propInterpretorCompilerEqual"
--      $ quickCheckWith stdArgs { maxSuccess = 1 } propInterpretorCompilerEqual
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
