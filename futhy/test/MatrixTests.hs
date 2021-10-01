module MatrixTests
  ( matrixTests
  )
where

import Prelude hiding (not)
import Test.Tasty.HUnit
import Test.Tasty
import Types
import Numeric.LinearAlgebra hiding ((|>))
import Data.Maybe
import Flow
import Control.Monad
import Matrix

goodCaseMatrix :: TestName -> LFun -> LFun -> Shape -> TestTree
goodCaseMatrix name vin vout shp = testCase (" " ++ name)
                                <| lfun2mtcs vin shp @?= lfun2mtcs vout shp


goodCompactMatrix :: TestName -> LFun -> LFun -> Shape -> TestTree
goodCompactMatrix name vin vout shp = testCase (" " ++ name)
          <| map (`compaction` shp)  (lfun2mtcs vin shp) @?= map (`compaction` shp) (lfun2mtcs vout shp)


-- This function is inspired by the AP exam.
not _ name vin vout shp =
      let act = lfun2mtcs vin shp
          exp = lfun2mtcs vout shp
       in testCase ("*" ++ name)
            <| when (act == exp)
            <| assertFailure "Unexpected Success.  The values are supposed to be distinct."

{- Example values -}

a = Tensor [ Tensor [Scalar 0, Scalar 1]
           , Tensor [Scalar 2, Scalar 3]
           ]
b = Tensor [ Tensor [Scalar 2, Scalar 3]
           , Tensor [Scalar 5, Scalar 7]
           ]
c = Tensor [ Tensor [Scalar 23, Scalar 31]
           , Tensor [Scalar 57, Scalar 74]
           ]
-- Hand calculation:
ab = Tensor [ Tensor [Scalar  5, Scalar  7]
            , Tensor [Scalar 19, Scalar 27]
            ]

ba = Tensor [ Tensor [Scalar  5, Scalar  7]
            , Tensor [Scalar 19, Scalar 27]
            ]

neye = Tensor [ Tensor [Scalar (-1), Scalar 0   ]
              , Tensor [Scalar 0,    Scalar (-1)]
              ]

eye = Tensor [ Tensor [Scalar 1, Scalar 0]
             , Tensor [Scalar 0, Scalar 1]
             ]

diag818 = Tensor [ Tensor [Scalar 818, Scalar 0]
                 , Tensor [Scalar 0, Scalar 818]
                 ]

zero = Tensor [ Tensor [Scalar 0, Scalar 0]
              , Tensor [Scalar 0, Scalar 0]
              ]
shp2x2 = (2,2)

matrixTests :: TestTree
matrixTests =
  testGroup "Matrixification of LFuns"
    [ goodCaseMatrix "Ident"
      Id
      (LSec eye MatrixMult)
      shp2x2
    , goodCaseMatrix "Neg"
      Neg
      (LSec neye MatrixMult)
      shp2x2
    , goodCaseMatrix "Scale 818"
      (Scale 818)
      (LSec diag818 MatrixMult)
      shp2x2
    , goodCaseMatrix "KZero"
      KZero
      (LSec zero MatrixMult)
      shp2x2
    , goodCompactMatrix "Compaction of two LSecs: \\v -> A(Bv) ==> \\v -> (AB)v"
      (Comp (LSec a MatrixMult) (LSec b MatrixMult))
      (LSec ab MatrixMult)
      shp2x2
    , not goodCompactMatrix "Compaction (matmul) is not commutative: \\v -> (BA)v =/> \\v -> (AB)v"
      (Comp (LSec b MatrixMult) (LSec a MatrixMult))
      (LSec ab MatrixMult)
      shp2x2
    , goodCompactMatrix "Compaction of RSec: \\v -> (vA)B ==> \\v -> v(AB)"
      (Comp (RSec MatrixMult a) (RSec MatrixMult b))
      (RSec MatrixMult ab)
      shp2x2
    , goodCompactMatrix "Two-sided compactions: \\v -> ABvBA ==> \\v -> (AB)v(BA)"
      (Comp
        (Comp (LSec a MatrixMult) (LSec b MatrixMult))
        (Comp (RSec MatrixMult a) (RSec MatrixMult b))
      )
      (Comp
        (LSec ab MatrixMult)
        (RSec MatrixMult ba)
      )
      shp2x2
    , testCase " Partition LSec's/RSec's: \\v -> RLRRLLRLLv ==> \\v -> LLLLLvRRRR"
      <| lfun2mtcs
      (Comp
        (RSec MatrixMult a)
        (Comp
          (LSec b MatrixMult)
          (Comp
            (RSec MatrixMult c)
            (Comp
              (RSec MatrixMult a)
              (Comp
                (LSec b MatrixMult)
                (Comp
                  (LSec c MatrixMult)
                  (Comp
                    (RSec MatrixMult a)
                    (Comp
                      (LSec b MatrixMult)
                      (LSec c MatrixMult)
                    )
                  )
                )
              )
            )
          )
        )
        )
      shp2x2
      @?=
          [(map (`tnsr2mtx` shp2x2) [b,b,c,b,c], map (`tnsr2mtx` shp2x2) [a,c,a,a])]
      ]
