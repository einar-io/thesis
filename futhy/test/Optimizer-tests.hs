import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Optimizer
import Types

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

goodCase :: TestName -> LFun -> LFun -> TestTree
goodCase name vin vout = testCase name $ optimize vin @?= vout

tests :: TestTree
tests = testGroup "Optimizer"
  [ testGroup "branch tests"
    [ goodCase "Comp Id Id -> Id"
      (Comp (Comp Id (Comp Id Id)) (Comp Id Id))
      (Id)
    , goodCase "Combining many scales"
      (Comp (Comp (Scale 2.0) (Comp (Scale 2.0) (Comp (Comp (Scale 2.0) (Comp (Scale 2.0) (Scale 2.0))) (Comp (Scale 2.0) (Scale 2.0))))) (Comp (Scale 2.0) (Scale 2.0)))
      (Scale 512.0)
    , goodCase "Combining many paras"
      (Comp (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Comp (Para (Scale 2.0) (Scale 2.0)) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0)))) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0)))))) (Comp (Para (Scale 2.0) (Scale 2.0)) (Para (Scale 2.0) (Scale 2.0))))
      (Para (Scale 512.0) (Scale 512.0))
    ]
  ]
