module Executer
  ( runFile
  , runStr
  , runStrArg
  )
where

import Types
import System.IO (openTempFile, hClose, stderr, hPrint)
import System.Process (readProcessWithExitCode, showCommandForUser)
import System.FilePath (dropExtension)
import Control.Monad.Reader
import Control.Monad.Except (throwError)
-- import GHC.IO.Exception (ExitCode(..))
import Flow
import Data.Time.Clock

p :: String -> Command ()
p s =
  let debug = False
  in when debug (liftIO <| hPrint stderr s)

getTime :: Command TimeStamp
getTime = liftIO getCurrentTime

makeLog :: CommandOutput -> TimeStamp -> TimeStamp -> Log
makeLog (exitcode2, stdout2, stdin2) begin_time finish_time = Log
  { exitcode = exitcode2
  , stdout   = stdout2
  , stdin    = stdin2
  , begin    = begin_time
  , finish   = finish_time
  }

runCommand :: String -> [Char] -> [String] -> FilePath -> [Char] -> Command Result
runCommand arg cxt params executable thingtodo = do
  p $ cxt <> " Command going to be run: " ++ showCommandForUser executable params <> arg

  begin_time <- getTime
  output@(exitcode2, stdout2, stdin2) <- liftIO <| readProcessWithExitCode executable params arg
  when (isExitFailure exitcode2)                <| throwError (CommandFailure ExecutionError output)
  finish_time <- getTime

  p $ cxt <> " " <> thingtodo <> " results:"
  p $ cxt <> " ExitCode: " ++ show exitcode2
  p $ cxt <> " stdout:   " ++ show stdout2
  p $ cxt <> " stdin :   " ++ show stdin2
  p $ cxt <> " " <> thingtodo <> " ENDED"
  let current_log = makeLog output begin_time finish_time
  return (CommandResult current_log)

-- |Compile the Futhark source code in env.
compile :: Command Result
compile = do
  Env filepath backend <- ask
  runCommand "" "[Futhark]" [show backend, filepath] "futhark" "Compilation"

--- but with std'ins
executeArg :: StdInArg -> Command Result
executeArg arg = do
  filepath <- asks fp
  runCommand (" " <> arg) "[LinPgm]" [] (dropExtension filepath) "Execution"

-- |Execute the compiled Futhark executable 'futExec' containing the compiled linear program.
makeTemp :: Command FutPgmFile
makeTemp = do
  let path   = "build/"
  let prefix = "autogenerated_.fut"
  (filepath, handle) <- liftIO $ openTempFile path prefix
  liftIO $ hClose handle
  p filepath
  return filepath

writeTemp :: FutPgmStr -> Command ()
writeTemp futStr = do
  filepath <- asks fp
  liftIO $ writeFile filepath futStr

store :: FutPgmStr -> Command FutPgmFile
store futPgmStr = do
  filepath <- makeTemp
  backend <- asks be
  let envNew = Env { fp = filepath, be = backend }
  local (const envNew) (writeTemp futPgmStr)
  return filepath


runStrArgM :: FutPgmStr -> StdInArg -> Command Result
runStrArgM futPgmStr arg = do
  filepath <- store futPgmStr
  backend <- asks be
  let envNew = Env { fp = filepath, be = backend }
  local (const envNew) (runFileArgM arg)

runFileArgM :: StdInArg -> Command Result
runFileArgM arg = compile >> executeArg arg

------ interface
runStr :: FutPgmStr -> Backend -> IO (CommandExecution Result)
runStr futPgmStr backend = runStrArg futPgmStr backend "\n"

runFile :: FutPgmFile -> Backend -> IO (CommandExecution Result)
runFile futPgmFile backend =
  let envInit = Env { fp = futPgmFile, be = backend }
  in execCmd (runFileArgM "\n") envInit

runStrArg :: FutPgmStr -> Backend -> StdInArg -> IO (CommandExecution Result)
runStrArg futPgmStr backend arg =
  let envInit = Env { fp = "", be = backend }
  in execCmd (runStrArgM futPgmStr arg) envInit
