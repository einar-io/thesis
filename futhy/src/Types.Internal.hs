module Types.Internal where

-- error types!
data Error
  = Something String
  deriving (Show, Eq)
