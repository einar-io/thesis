module Executor 
  ( FutFile
  , FutStr
  , runFile
  , runStr)
  where

import Executor.Internal 
import Types.Internal

testExec = runStr "entry main = reduce (+) 0 [1,2,3,4,5,6]" C
