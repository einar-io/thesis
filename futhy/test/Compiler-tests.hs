import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Types
import Utils
import Compiler

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

just_lfun :: LFun -> String
just_lfun f = let (_, r, _) = lfun f 1 (Atom 0) in r

goodCaseProgram :: TestName -> LFun -> Val -> String -> TestTree
goodCaseProgram name lf vin vout = testCase name $ program lf vin @?= vout

goodCaseVal :: TestName -> Val -> String -> TestTree
goodCaseVal name vin vout = testCase name $ val vin @?= vout

goodCaseLFun :: TestName -> LFun -> String -> TestTree
goodCaseLFun name lf vout = testCase name $ just_lfun lf @?= vout

-- TODO
  -- use quickcheck? to test that compiler + executor gives same result as interpretor
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
              "([[3.0f32, 6.0f32], [9.0f32, 12.0f32]],[[3.0f32, 6.0f32], [9.0f32, 12.0f32]])"
            ]
          , testGroup "lfun tests"
            [ goodCaseLFun "Id"
              (Id)
              "let fun1 = (id)\n"
            , goodCaseLFun "Id"
              (Dup)
              "let fun1 = (dupe)\n"
            , goodCaseLFun "scale"
              (Scale 3.0)
              "let fun1 = (outer_0_0 3.0f32)\n"
            , goodCaseLFun "comp"
              (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
              "let fun3 = (outer_0_0 3.0f32)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun4 = (comp fun3 fun2)\nlet fun1 = (outer_0_0 8.0f32)\nlet fun5 = (comp fun4 fun1)\n"
            , goodCaseLFun "para"
              (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
              "let fun3 = (outer_0_0 3.0f32)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun4 = (para fun3 fun2)\nlet fun1 = (dupe)\nlet fun5 = (comp fun4 fun1)\n"
            , goodCaseLFun "lplus"
              (Comp  (Lplus  (Scale 3.0) (Scale 7.0)) Dup)
              "let fun3 = (outer_0_0 3.0f32)\nlet fun2 = (outer_0_0 7.0f32)\nlet fun4 = (para fun3 fun2)\nlet fun5 = (plus_0_0 fun4)\nlet fun1 = (dupe)\nlet fun6 = (comp fun5 fun1)\n"
            ]
          , testGroup "whole program tests"
            [ goodCaseProgram "Id scalar"
              (Id)
              (Scalar 15.0)
              "open import \"lmaplib\"\n\nlet arg = 15.0f32\n\nlet fun1 = (id)\nentry main = fun1 arg"
            , goodCaseProgram "complex"
              (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Scale 7.0)) Dup)) Dup)
              (Tensor [Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]])
              "open import \"lmaplib\"\n\nlet arg = [[1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32]]\n\nlet fun1 = (dupe)\nlet fun2 = (dupe)\nlet fun3 = (outer_0_2 7.0f32)\nlet fun4 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet fun5 = (para fun4 fun3)\nlet fun6 = (comp fun5 fun2)\nlet fun7 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet fun8 = (para fun7 fun6)\nlet fun9 = (comp fun8 fun1)\nentry main = fun9 arg"
            ]
          ]
