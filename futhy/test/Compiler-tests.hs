import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Types
import Compiler

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

just_lfun :: LFun -> String
just_lfun f = snd $ lfun f 1

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
              "let FUN1 = (sv 3.0)\n"
            , goodCaseLFun "comp"
              (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
              "let FUN1 = (sv 3.0)\nlet FUN2 = (sv 7.0)\nlet FUN3 = (comp FUN1 FUN2)\nlet FUN4 = (sv 8.0)\nlet FUN5 = (comp FUN3 FUN4)\n"
            , goodCaseLFun "para"
              (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
              "let FUN1 = (sv 3.0)\nlet FUN2 = (sv 7.0)\nlet FUN3 = (para FUN1 FUN2)\nlet FUN4 = (dupe)\nlet FUN5 = (comp FUN3 FUN4)\n"
            ]
          , testGroup "whole program tests"
            [ goodCaseProgram "Id scalar"
              (Id)
              (Scalar 15.0)
              "open import \"lmaplib\"\n\nlet ARG = 15.0f32\n\nlet FUN1 = (id)\n\nentry main = FUN1 ARG"
            ]
          ]
