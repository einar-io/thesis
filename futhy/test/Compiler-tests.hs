import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Types
import Compiler
import Utils

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

goodCaseProgram :: TestName -> LFun -> Val -> String -> TestTree
goodCaseProgram name lf vin vout = testCase name $ compileProgram lf (val_arity vin) @?= vout

goodCaseVal :: TestName -> Val -> String -> TestTree
goodCaseVal name vin vout = testCase name $ val vin @?= vout

tests :: TestTree
tests = testGroup "compiler"
          [ testGroup "literal val tests"
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
