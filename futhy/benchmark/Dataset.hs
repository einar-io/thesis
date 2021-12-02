module Dataset
  ( genVector
  , genVectors
  , initDatasets
  ) where

import Types
import System.IO (openTempFile, hClose, stderr, hPutStrLn)
import System.Process (readCreateProcessWithExitCode, shell)
import System.Directory (doesFileExist, getFileSize)
import Control.Monad
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class
import Data.List
import Flow
import Executer (makeLog)

seed :: Int
seed = 53241

genVector :: Filepath -> Int -> IO Result
genVector filepath len = do

  {-
  -- Do not generate files if they exist and are non-zero.

  fileExists <- liftIO <| doesFileExist filepath

  fileSize <- if fileExists then getFileSize filepath else return 0

  guard (fileSize == 0) -- only proceed to generate dataset when file is missing or 0
  -}

  let executable = "futhark"
  -- Relevant documentation
  -- https://futhark.readthedocs.io/en/latest/man/futhark-dataset.html
  let { params =
        [ "dataset"
        , "--binary"
        , "--generate [" ++ show len ++ "]f32"
        , "-s " ++ show seed
        , "> "  ++ filepath
        ]
      }

  let shellCmd = shell <| concat <| intersperse " " (executable:params)

  --print $ "[Dataset] Command to be run: " ++ show shellCmd

  output@(_exitcode, _stdout, _stdin) <- liftIO <| readCreateProcessWithExitCode shellCmd ""
  --when (isExitFailure _exitcode)               <| throwError (CommandFailure ExecutionError output)

--  print   "[Dataset] Execution done."

  let benchLog = makeLog output ""
  return (Result benchLog)

genVectors :: Filepath -> Int -> IO [Result]
genVectors name vecOrder =
  let vecLens = do i <- [1..vecOrder]
                   return (2^i)
      filePaths = do l <- vecLens
                     return <| "build/" ++ name ++ "_" ++ show l ++ ".val"
      pathsAndLens = zip filePaths vecLens
   in mapM (uncurry genVector) pathsAndLens

-- consider seed parameter
initDatasets :: Int -> IO ()
initDatasets oom = do
  _ <- genVectors "dataset" oom
  return ()
