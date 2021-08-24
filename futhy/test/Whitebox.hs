import Lib
import Test.Tasty
import Test.Tasty.HUnit
import Prelude

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "All tests"
  [inttests]

goodCase :: TestName -> LFun -> Var -> Var -> TestTree
goodCase name lfun vin vout = testCase name $ interpret lfun vin @?= vout

inttests :: TestTree
inttests = testGroup "interpretor"
  [ testGroup "branch tests"
    [ goodCase "Id scalar"
      (Id)
      (Scalar 15.0)
      (Scalar 15.0)
    , goodCase "Id vector"
      (Id)
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
    , goodCase "Scale scalar"
      (Scale 3.0)
      (Scalar 5.0)
      (Scalar 15.0)
    , goodCase "Scale vector"
      (Scale 3.0)
      (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 3.0, Scalar 6.0, Scalar 9.0])
    , goodCase "Scale matrix"
      (Scale 3.0)
      (Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]])
      (Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]])
    , goodCase "Scale R3 tensor"
      (Scale 3.0)
      (Tensor [Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]], Tensor [Tensor [Scalar 5.0, Scalar 6.0], Tensor [Scalar 7.0, Scalar 8.0]]])
      (Tensor [Tensor [Tensor [Scalar 3.0, Scalar 6.0], Tensor [Scalar 9.0, Scalar 12.0]], Tensor [Tensor [Scalar 15.0, Scalar 18.0], Tensor [Scalar 21.0, Scalar 24.0]]])
    , goodCase "Comp id and scale"
      (Comp Id (Scale 7.0))
      (Scalar 10.0)
      (Scalar 70.0)
    , goodCase "Comp scale scale"
      (Comp  (Scale (-2.0)) (Scale 7.0))
      (Scalar 10.0)
      (Scalar (-140.0))
    , goodCase "Comp scale scale scale on vector"
      (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 7.0))
      (Tensor [Scalar 1.0, Scalar 2.0])
      (Tensor [Scalar 147.0, Scalar 294.0])
    , goodCase "Outer product scalar scalar"
      (RSec (Scalar 2.0) Outer)
      (Scalar 2.0)
      (Scalar 4.0)
    , goodCase "Outer product vector scalar"
      (RSec (Tensor [Scalar 1.0, Scalar 2.0]) Outer)
      (Scalar 3.0)
      (Tensor [Scalar 3.0, Scalar 6.0])
    , goodCase "Outer product scalar vector"
      (RSec (Scalar 2.0) Outer)
      (Tensor [Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 4.0, Scalar 6.0])
    , goodCase "Outer product vector vector"
      (RSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0], Tensor [Scalar 12.0, Scalar 15.0]])
    , goodCase "2 {outer product} 3 -> 2x3"
      (RSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0, Scalar 12.0], Tensor [Scalar 12.0, Scalar 15.0, Scalar 18.0]])
    , goodCase "3 {outer product} 2 -> 3x2"
      (LSec Outer (Tensor [Scalar 2.0, Scalar 3.0]))
      (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 12.0], Tensor [Scalar 10.0, Scalar 15.0], Tensor [Scalar 12.0, Scalar 18.0]])
    ]
  ]
