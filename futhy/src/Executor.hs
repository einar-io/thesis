module Executor
  ( readFut
  , execute
  ) where

import Compiler
import Types
import Types.Internal
import System.IO (readFile)
import System.Process (readProcessWithExitCode, showCommandForUser)

-- Take output from compiler and make futhark run it with C backend, and get the result

-- |Read the `.fut` file at `FilePath`.
readFut :: FilePath -> IO Fut
readFut fp = do
  s <- System.IO.readFile fp
  return (s :: Fut)

-- |Run the program `fut`.
-- |TODO: Split into compile and compute
execute :: Fut -> IO String
execute fut = do
  let  futexec = "futhark"
  let futparams = ["c", fut]
  putStrLn $ showCommandForUser futexec futparams
  (exitcode, stdout, _stdin) <- readProcessWithExitCode futexec futparams ""
  print exitcode
  print stdout
  print "Futhark Compiler DONE"
  (exitcode, stdout, _stdin) <- readProcessWithExitCode  "./test/fut/executor" [] "\n"
  print exitcode
  print stdout
  print "Executing DONE"
  return $ show exitcode



