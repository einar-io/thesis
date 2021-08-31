module Types.Internal where

import Types

-- error types!
data Error
  = Something String
  deriving (Show, Eq)

type Filename = String
type Fut = String
type Derivative = Val
