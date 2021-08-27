import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Types
import Interpretor
import Compiler
import Executor

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup "All tests"
  [inttests]

goodCase :: TestName -> LFun -> Val -> Val -> TestTree
goodCase name lfun vin vout = testCase name $ interpret lfun vin @?= vout

-- TODO
  -- use quickcheck to test that compiler + executor gives same result as interpretor
inttests :: TestTree
inttests = testGroup "interpretor"
  [ testGroup "branch tests"
    [ goodCase "Id scalar"
      (Id)
      (Scalar 15.0)
      (Scalar 15.0)
    ]
  ]
