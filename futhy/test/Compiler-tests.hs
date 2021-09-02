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
              "let FUN1 = (id)\n"
            , goodCaseLFun "Id"
              (Dup)
              "let FUN1 = (dupe)\n"
            , goodCaseLFun "scale"
              (Scale 3.0)
              "let FUN1 = (outer_0_0 3.0f32)\n"
            , goodCaseLFun "comp"
              (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
              "let FUN3 = (outer_0_0 3.0f32)\nlet FUN2 = (outer_0_0 7.0f32)\nlet FUN4 = (comp FUN3 FUN2)\nlet FUN1 = (outer_0_0 8.0f32)\nlet FUN5 = (comp FUN4 FUN1)\n"
            , goodCaseLFun "para"
              (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
              "let FUN3 = (outer_0_0 3.0f32)\nlet FUN2 = (outer_0_0 7.0f32)\nlet FUN4 = (para FUN3 FUN2)\nlet FUN1 = (dupe)\nlet FUN5 = (comp FUN4 FUN1)\n"
            ]
          , testGroup "whole program tests"
            [ goodCaseProgram "Id scalar"
              (Id)
              (Scalar 15.0)
              "open import \"lmaplib\"\n\nlet ARG = 15.0f32\n\nlet FUN1 = (id)\n\nentry main = FUN1 ARG"
            , goodCaseProgram "complex"
              (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Scale 7.0)) Dup)) Dup)
              (Tensor [Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]])
              "open import \"lmaplib\"\n\nlet ARG = [[1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32]]\n\nlet FUN7 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet FUN4 = (outer_1_2 [1.0f32, 2.0f32, 3.0f32])\nlet FUN3 = (outer_0_2 7.0f32)\nlet FUN5 = (para FUN4 FUN3)\nlet FUN2 = (dupe)\nlet FUN6 = (comp FUN5 FUN2)\nlet FUN8 = (para FUN7 FUN6)\nlet FUN1 = (dupe)\nlet FUN9 = (comp FUN8 FUN1)\n\nentry main = FUN9 ARG"
            ]
          ]
