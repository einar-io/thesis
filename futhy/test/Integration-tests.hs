import Test.Tasty.HUnit
import Test.Tasty
import Prelude

-- our libs
import Types
import Compiler
import Interpretor
import Utils
import Executor hiding (main)

second :: Integer
second = 1000000

main :: IO ()
main = defaultMain $ localOption (mkTimeout $ second * 10) tests

goodCase :: TestName -> LFun -> Val -> TestTree
goodCase name lf vin = testCase name $ do 
                                        result <- (runStrArg (val vin) (compileProgram lf (val_arity vin)) C)
                                        reference <- runStr ("entry main = " <> (val (interpret lf vin))) C
                                        case (result, reference) of
                                          (Right (_, res, _), Right (_, ref, _)) -> res @?= ref
                                          _ -> result @?= reference

tests :: TestTree
tests = testGroup "unified tests"
          [ goodCase "Id"
            (Id)
            (Scalar 1.0)
          , goodCase "Dupe"
            (Dup)
            (Scalar 1.0)
          , goodCase "scale scalar"
            (Scale 3.0)
            (Scalar 1.0)
          , goodCase "scale marix"
            (Scale 3.0)
            (Tensor [Tensor [Scalar 1.0, Scalar 2.0], Tensor [Scalar 3.0, Scalar 4.0]])
          , goodCase "comp"
            (Comp  (Comp  (Scale 3.0) (Scale 7.0)) (Scale 8.0))
            (Scalar 1.0)
          , goodCase "para"
            (Comp  (Para  (Scale 3.0) (Scale 7.0)) Dup)
            (Scalar 1.0)
          , goodCase "lplus"
            (Comp  (Lplus  (Scale 3.0) (Scale 7.0)) Dup)
            (Scalar 1.0)
          , goodCase "Id scalar"
            (Id)
            (Scalar 15.0)
          , goodCase "complex"
            (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Comp  (Para  (LSec (Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]) Outer ) (Scale 7.0)) Dup)) Dup)
            (Tensor [Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0], Tensor [Scalar 1.0, Scalar 2.0, Scalar 3.0]])
          ]
