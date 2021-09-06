module Executor
  ( runFile
  , runStr
  , main)
where

import Executor.InternalM
import Types

-- TO BE REMOVED: for testying purposes only.
main :: IO (Either ExecutionError ExecutionResult)
main = runStr "entry main = reduce (+) 0 [1,2,3,4,5,6]" C
