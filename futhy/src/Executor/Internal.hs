module Executor.Internal where

import Types()
import Types.Internal
import System.IO (readFile)
import System.Process (readProcessWithExitCode, showCommandForUser)
import System.FilePath (dropExtension)

-- |Read the `.fut` file at `FilePath`.
readFut :: FilePath -> IO FutStr
readFut fp = do
  s <- System.IO.readFile fp
  return (s :: FutStr)

-- |Compile the Futhark source code in file 'fut'.
compileFutFile :: FutFile -> Backend -> IO CommandResult
compileFutFile fut bck = do
  let   backend = show bck
  let   futexec = "futhark"
  let futparams = [backend, fut]
  putStrLn $ "[Futhark] Command going to be run: " ++ showCommandForUser futexec futparams
  res <- readProcessWithExitCode futexec futparams ""
  let (exitcode, stdout, stdin) = res
  print   "[Futhark] Compilation results:"
  print $ "[Futhark] ExitCode: " ++ show exitcode
  print $ "[Futhark] stdout:   " ++ show stdout
  print $ "[Futhark] stdin :   " ++ show stdin
  print   "[Futhark] Compilation COMPLETED"
  return (show exitcode, stdout, stdin)

-- |Execute the compiled Futhark executable 'futExec' containing the compiled linear program.
exec :: FutExec -> IO CommandResult
exec futExec = do
  let params = []
  print $ "[LinPgm] Command going to be run: " ++ showCommandForUser futExec params
  res <- readProcessWithExitCode futExec params "\n"
  let (exitcode, stdout, stdin) = res
  print   "[LinPgm] Execution results:"
  print $ "[LinPgm] ExitCode: " ++ show exitcode
  print $ "[LinPgm] stdout:   " ++ show stdout
  print $ "[LinPgm] stdin :   " ++ show stdin
  print   "[LinPgm] Execution ENDED"
  return (show exitcode, stdout, stdin)

-- |Run a Futhark program file by compiling it and running the result.
runFile :: FutFile -> Backend -> IO CommandResult
runFile futFile bck = do
  _ <- compileFutFile futFile bck
  let executable = dropExtension futFile
  exec executable

-- |Run a Futhark program string by saving it to a file and 'runFile' it.
runStr :: FutStr -> Backend -> IO CommandResult
runStr futStr backend = do
  let fp = "../build/temp.fut"
  writeFile fp futStr
  runFile fp backend




{-
-- |DEPRECATED
-- |Run the program `fut`.
-- |TODO: Split into compile and compute
execute :: FutFile -> IO String
execute fut = do
  let  futexec = "futhark"
  let futparams = ["c", fut]
  putStrLn $ showCommandForUser futexec futparams
  (exitcode, stdout, _stdin) <- readProcessWithExitCode futexec futparams ""
  print exitcode
  print stdout
  print "Futhark Compiler DONE"
  putStrLn $ showCommandForUser  "./test/fut/executor" futparams
  (exitcode, stdout, _stdin) <- readProcessWithExitCode  "./test/fut/executor" [] "\n"
  print exitcode
  print stdout
  print "Executing DONE"
  return $ show exitcode
-}
