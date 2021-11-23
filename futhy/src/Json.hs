module Json
  ( main
  , json2series
  ) where

import Types
import Data.Aeson

main :: IO ()
main = do
  let series = json2series "{}"
  print series

json2series :: Json -> [[Float]]
json2series json = undefined
