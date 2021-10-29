module ReduceTests (reduceTests) where

import Types

reduceTests :: [([Char], LFun, Val, Val)]
reduceTests =
  [ ("Reduce primitive test: happy path"
        , Red (List [(0,1)])
        , Tensor [Scalar 0]
        , Tensor [Scalar 0, Scalar 0]
        )
  , ("Reduce primitive test: longer relation"
        , Red (List [(0,0),(1,1),(2,2),(3,3)])
        , Tensor [Scalar 0, Scalar 1, Scalar 2, Scalar 3]
        , Tensor [Scalar 0, Scalar 1, Scalar 2, Scalar 3]
        )
  , ("Reduce primitive test: unused associations"
        , Red (List [(0,1), (2,3), (4,5)])
        , Tensor [Scalar 1]
        , Tensor [Scalar 0, Scalar 1, Scalar 0, Scalar 0, Scalar 0, Scalar 0]
        )
  , ("Reduce primitive test: duplicates."
        , Red (List [(1,2), (1,2), (1,1), (1,2)])
        , Tensor [Scalar 1, Scalar 1, Scalar 1, Scalar 2, Scalar 3]
        , Tensor [Scalar 0, Scalar 1, Scalar 3]
        )
        --, Tensor [(1, 1), (2, 3)])
  , ("Reduce compound test: duplicates and out-of-bounds indices."
        , Red (List [(1,2), (0,2), (1,3), (5,6)])
        , Tensor [Scalar (-1), Scalar 1, Scalar 2, Scalar 3]
        , Tensor [Scalar 0, Scalar 0, Scalar 0, Scalar 1, Scalar 0, Scalar 0, Scalar 0]
        --, Tensor [(3, 1), (2, 0)]
        )
  , ("Reduce compound test: negative indices."
        , Red (List [(1,2), (0,2), (1,3), (5,6)])
        , Tensor [Scalar (-1), Scalar (-2), Scalar (-3)]
        , Tensor [Scalar 0, Scalar 0, Scalar (-3), Scalar (-2), Scalar 0, Scalar 0, Scalar 0]
        --, Tensor [(3, 1), (2, 0)]
        )
  ]

