{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Executor.Internal where

import Types
import System.IO (readFile, openTempFile, openTempFileWithDefaultPermissions, hClose)
import System.Process (readProcessWithExitCode, showCommandForUser)
import System.FilePath (dropExtension)


-- |Read the `.fut` file at `FilePath`.
readFut :: FilePath -> IO FutPgmStr
readFut fp = do
  s <- System.IO.readFile fp
  return (s :: FutPgmStr)

-- |Compile the Futhark source code in file 'futFile'.
compileFutFile :: FutPgmFile -> Backend -> IO ExecutionResult
compileFutFile futFile bck = do
  let   backend = show bck
  let   futexec = "futhark"
  let futparams = [backend, futFile]
  putStrLn $ "[Futhark] Command going to be run: " ++ showCommandForUser futexec futparams
  er <- readProcessWithExitCode futexec futparams ""
  let (exitcode, stdout, stdin) = er
  print   "[Futhark] Compilation results:"
  print $ "[Futhark] ExitCode: " ++ show exitcode
  print $ "[Futhark] stdout:   " ++ show stdout
  print $ "[Futhark] stdin :   " ++ show stdin
  print   "[Futhark] Compilation COMPLETED"
  return er

-- |Execute the compiled Futhark executable 'futExec' containing the compiled linear program.
execFutExec :: FutPgmExec -> IO ExecutionResult
execFutExec futExec = do
  let params = []
  print $ "[LinPgm] Command going to be run: " ++ showCommandForUser futExec params
  er <- readProcessWithExitCode futExec params "\n"
  let (exitcode, stdout, stdin) = er
  print   "[LinPgm] Execution results:"
  print $ "[LinPgm] ExitCode: " ++ show exitcode
  print $ "[LinPgm] stdout:   " ++ show stdout
  print $ "[LinPgm] stdin :   " ++ show stdin
  print   "[LinPgm] Execution ENDED"
  return er

-- |Run a Futhark program file by compiling it and running the result.
runFile :: FutPgmFile -> Backend -> IO ExecutionResult
runFile futFile bck = do
  _ <- compileFutFile futFile bck
  let executable = dropExtension futFile
  execFutExec executable

-- |Run a Futhark program string by saving it to a temporary file and 'runFile' it.
runStr :: FutPgmStr -> Backend -> IO ExecutionResult
runStr futStr backend = do
  let path = "build/"
  let  prefix = "autogenerated_.fut"
  (fp, hdl) <- openTempFile path prefix
  hClose hdl
  print fp
  writeFile fp futStr
  runFile fp backend

makeTempFutFile :: IO String
makeTempFutFile = do
  let path   = "build/"
  let prefix = "autogenerated_.fut"
  (filepath, hdl) <- openTempFile path prefix
  hClose hdl
  print filepath
  return filepath

