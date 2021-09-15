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
main = defaultMain $ localOption (mkTimeout $ second * 5) tests

goodCaseOptimizer :: TestName -> LFun -> LFun -> TestTree
goodCaseOptimizer name vin vout = testCase name $ optimize vin @?= vout

goodCaseInterpretor :: TestName -> LFun -> Val -> Val -> TestTree
goodCaseInterpretor name lfun vin vout = testCase name $ interpret lfun vin @?= return vout

goodCaseProgram :: TestName -> LFun -> Val -> String -> TestTree
goodCaseProgram name lf vin vout = testCase name $ compileProgram lf (val_arity vin) @?= vout

goodCaseVal :: TestName -> Val -> String -> TestTree
goodCaseVal name vin vout = testCase name $ show vin @?= vout

goodCaseExecution :: TestName -> LFun -> Val -> TestTree
goodCaseExecution name lf vin =
  testCase name $ do
                     compileRes <- runStrArg (compileProgram lf (val_arity vin)) C (show vin)
                     resStr  <- case compileRes of
                                Right (Output (_, res, _)) -> return res
                                e -> assertFailure $ show e
                     let Right intVal = interpret lf vin
                     intComp <- runStr ("entry main = " <> show intVal) C
                     refStr <-  case intComp of
                                Right (Output (_, ref, _)) -> return ref
                                e -> assertFailure $ show e
                     case (resStr == refStr) of
                      False -> assertFailure $ show (resStr, refStr)
                      True -> return ()

tests :: TestTree
tests =
  testGroup "All tests"
    [ optimizorTests
    , interpretorTests
    , compilorTests
    , executorTests
--    , qcTests
    ]

optimizorTests :: TestTree
optimizorTests =
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

interpretorTests :: TestTree
interpretorTests =
    testGroup "Interpretor"
    [ goodCaseInterpretor "Id scalar"
      (Id)
      (Scalar 15.0)
      (Scalar 15.0)
    , goodCaseInterpretor "Id vector"
      (Id)
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
    , goodCaseInterpretor "Scale scalar"
      (Scale 3.0)
      (Scalar 5.0)
      (Scalar 15.0)
    , goodCaseInterpretor "Scale vector"
      (Scale 3.0)
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0])
    , goodCaseInterpretor "Scale matrix"
      (Scale 3.0)
      (Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]])
      (Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]])
    , goodCaseInterpretor "Scale R3 tensor"
      (Scale 3.0)
      (Tensor [Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]], Tensor [Tensor [Scalar 5.0, Scalar 6.0], Tensor [Scalar 7.0, Scalar 8.0]]])
      (Tensor [Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]], Tensor [Tensor [Scalar 15.0, Scalar 18.0], Tensor [Scalar 21.0, Scalar 24.0]]])
    , goodCaseInterpretor "Comp id and scale"
      (Comp Id (Scale 7.0))
      (Scalar 10.0)
      (Scalar 70.0)
    , goodCaseInterpretor "Comp scale scale"
      (Comp  (Scale (-2.0)) (Scale 7.0))
      (Scalar 10.0)
      (Scalar (-140.0))
    , goodCaseInterpretor "Comp scale scale scale on vector"
      (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
      (Tensor [Scalar 1.0, Scalar 2.0])
      (Tensor [Scalar 168.0, Scalar 336.0])
    , goodCaseInterpretor "Outer product scalar scalar"
      (LSec (Scalar 2.0) Outer)
      (Scalar 2.0)
      (Scalar 4.0)
    , goodCaseInterpretor "Outer product vector scalar"
      (LSec (Tensor [Scalar 1.0, Scalar 2.0]) Outer)
      (Scalar 3.0)
      (Tensor [Scalar 3.0, Scalar 6.0])
    , goodCaseInterpretor "Outer product scalar vector"
      (LSec (Scalar 2.0) Outer)
      (Tensor [Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 4.0, Scalar 6.0])
    , goodCaseInterpretor "Outer product vector vector"
      (LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0], Tensor [Scalar 12.0, Scalar 15.0]])
    , goodCaseInterpretor "2 {outer product} 3 -> 2x3"
      (LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0, Scalar 12.0], Tensor [Scalar 12.0, Scalar 15.0, Scalar 18.0]])
  , goodCaseInterpretor "3 {outer product} 2 -> 3x2"
    (RSec Outer (Tensor [Scalar 2.0, Scalar 3.0]))
    (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
    (Tensor [Tensor [Scalar 8.0, Scalar 12.0], Tensor [Scalar 10.0, Scalar 15.0], Tensor [Scalar 12.0, Scalar 18.0]])
  , goodCaseInterpretor "Id (+) K0 (3.0, 5.0) -> (3.0, 0.0)"
    (Para Id KZero)
    (Pair (Scalar 3.0) (Scalar 5.0))
    (Pair (Scalar 3.0) (Scalar 0.0))
  , goodCaseInterpretor "Id ^+ K0 6.0 -> 6.0"
    (Lplus Id KZero)
    (Scalar 6.0)
    (Scalar 6.0)
  , goodCaseInterpretor "(Scale (-3.0)) ^+ (Scale 5.0) 7.0 -> 14.0"
    (Lplus (Scale (-3.0)) (Scale 5.0))
    (Scalar 7.0)
    (Scalar 14.0)
  ]



compilorTests :: TestTree
compilorTests =
    testGroup "compiler"
    [ testGroup "literal show tests"
      [ goodCaseVal "scalar"
        (Scalar 15.0)
        "15.0f32"
      , goodCaseVal "tensor"
        (Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]])
        "[[3.0f32, 6.0f32], [9.0f32, 12.0f32]]"
      , goodCaseVal "pair"
        (Pair (Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]]) (Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]]))
        "([[3.0f32, 6.0f32], [9.0f32, 12.0f32]], [[3.0f32, 6.0f32], [9.0f32, 12.0f32]])"
      ]
      , testGroup "whole program tests"
      [ goodCaseProgram "Id"
        (Id)
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (id)\nentry main (input: f32) = fun1 input"
      , goodCaseProgram "Dupe"
        (Dup)
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (dupe)\nentry main (input: f32) = fun1 input"
      , goodCaseProgram "scale scalar"
        (Scale 3.0)
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (outer_0_0 3.0f32)\nentry main (input: f32) = fun1 input"
      , goodCaseProgram "scale marix"
        (Scale 3.0)
        (Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]])
        "open import \"lmaplib\"\n\nlet fun1 = (outer_0_2 3.0f32)\nentry main (input: [][]f32) = fun1 input"
      , goodCaseProgram "comp"
        (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (outer_0_0 8.0f32)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun3 = (outer_0_0 3.0f32)\nlet fun4 = (comp fun3 fun2)\nlet fun5 = (comp fun4 fun1)\nentry main (input: f32) = fun5 input"
      , goodCaseProgram "para"
        (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (dupe)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun3 = (outer_0_0 3.0f32)\nlet fun4 = (para fun3 fun2)\nlet fun5 = (comp fun4 fun1)\nentry main (input: f32) = fun5 input"
      , goodCaseProgram "lplus"
        (Comp  (Lplus  (Scale 3.0) (Scale 7.0)) Dup)
        (Scalar 1.0)
        "open import \"lmaplib\"\n\nlet fun1 = (dupe)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun3 = (outer_0_0 3.0f32)\nlet fun4 = (para fun3 fun2)\nlet fun5 = (plus_0_0 fun4)\nlet fun6 = (comp fun5 fun1)\nentry main (input: f32) = fun6 input"
      , goodCaseProgram "Id scalar"
        (Id)
        (Scalar 15.0)
        "open import \"lmaplib\"\n\nlet fun1 = (id)\nentry main (input: f32) = fun1 input"
      , goodCaseProgram "complex"
        (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Scale 7.0)) Dup)) Dup)
        (Tensor [Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]])
        "open import \"lmaplib\"\n\nlet fun1 = (dupe)\nlet fun2 = (dupe)\nlet fun3 = (outer_0_2 7.0f32)\nlet fun4 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet fun5 = (para fun4 fun3)\nlet fun6 = (comp fun5 fun2)\nlet fun7 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet fun8 = (para fun7 fun6)\nlet fun9 = (comp fun8 fun1)\nentry main (input: [][]f32) = fun9 input"
      ]
    ]

executorTests :: TestTree
executorTests =
  testGroup "comparison tests - reference is interpretor, result is compiler"
    [ goodCaseExecution "Id"
      (Id)
      (Scalar 1.0)
    , goodCaseExecution "Dupe"
      (Dup)
      (Scalar 1.0)
    , goodCaseExecution "scale scalar"
      (Scale 3.0)
      (Scalar 1.0)
    , goodCaseExecution "scale marix"
      (Scale 3.0)
      (Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]])
    , goodCaseExecution "comp"
      (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
      (Scalar 1.0)
    , goodCaseExecution "para"
      (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
      (Scalar 1.0)
    , goodCaseExecution "lplus"
      (Comp  (Lplus  (Scale 3.0) (Scale 7.0)) Dup)
      (Scalar 1.0)
    , goodCaseExecution "complex"
      (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Scale 7.0)) Dup)) Dup)
      (Tensor [Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]])
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
