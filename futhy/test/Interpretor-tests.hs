import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Interpretor
import Types

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

goodCase :: TestName -> LFun -> Val -> Val -> TestTree
goodCase name lfun vin vout = testCase name $ interpret lfun vin @?= vout

tests :: TestTree
tests = testGroup "interpretor"
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
      (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
      (Tensor [Scalar 1.0, Scalar 2.0])
      (Tensor [Scalar 168.0, Scalar 336.0])
    , goodCase "Outer product scalar scalar"
      (LSec (Scalar 2.0) Outer)
      (Scalar 2.0)
      (Scalar 4.0)
    , goodCase "Outer product vector scalar"
      (LSec (Tensor [Scalar 1.0, Scalar 2.0]) Outer)
      (Scalar 3.0)
      (Tensor [Scalar 3.0, Scalar 6.0])
    , goodCase "Outer product scalar vector"
      (LSec (Scalar 2.0) Outer)
      (Tensor [Scalar 2.0, Scalar 3.0])
      (Tensor [Scalar 4.0, Scalar 6.0])
    , goodCase "Outer product vector vector"
      (LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0], Tensor [Scalar 12.0, Scalar 15.0]])
    , goodCase "2 {outer product} 3 -> 2x3"
      (LSec (Tensor [Scalar 2.0, Scalar 3.0]) Outer)
      (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 10.0, Scalar 12.0], Tensor [Scalar 12.0, Scalar 15.0, Scalar 18.0]])
    , goodCase "3 {outer product} 2 -> 3x2"
      (RSec Outer (Tensor [Scalar 2.0, Scalar 3.0]))
      (Tensor [Scalar 4.0, Scalar 5.0, Scalar 6.0])
      (Tensor [Tensor [Scalar 8.0, Scalar 12.0], Tensor [Scalar 10.0, Scalar 15.0], Tensor [Scalar 12.0, Scalar 18.0]])
    , goodCase "Id (+) K0 (3.0, 5.0) -> (3.0, 0.0)"
      (Oplus Id Zero)
      (Pair (Scalar 3.0) (Scalar 5.0))
      (Pair (Scalar 3.0) (Scalar 0.0))
    , goodCase "Id ^+ K0 6.0 -> 6.0"
      (Lplus Id Zero)
      (Scalar 6.0)
      (Scalar 6.0)
    , goodCase "(Scale (-3.0)) ^+ (Scale 5.0) 7.0 -> 14.0"
      (Lplus (Scale (-3.0)) (Scale 5.0))
      (Scalar 7.0)
      (Scalar 14.0)
    ]
  ]
