module Main
  ( computeDerivative
  , main
  ) where


{-
This file is mostly disabled (in ruins) until the called API is more stable.
-}


{-
import System.IO (readFile)
import Data.Char (toUpper)
-}

import Options.Applicative

import Types
import ArgParser
{-
import Executor hiding (main)
import Compiler
import Optimizer
-}

-- Load as module
computeDerivative :: LFun -> Val -> DerivativeComputation CommandResult
computeDerivative _lfun _v = undefined
{-
computeDerivative :: LFun -> Val -> DerivativeComputation ExecutionResult
computeDerivative lfun v =
  let futpgm = compileProgram lfun v
  in runStr futpgm C
-}

-- Read from `stdin`, then print to `stdout`.
main :: IO ()
main = start =<< execParser opts
  where
    opts = info (cliparser <**> helper)
      ( fullDesc
     <> progDesc "tad is a virtual coprocessor to speed up derivative computations for autodiff."
     <> header "tad - a virtual coprocessor to compute linear functions."
     <> footer "Thesis project by Ulrik Elmelund & Einar Rasmussen." )

-- Starts computation according to the supplied arguments
start :: Opts -> IO ()
start _  = undefined
{-
start (Opts infile outfile backend False False False ) = do
  lfun <- readFile infile
  runFile (compileProgram lfun val) (read (toUpper backend) :: Backend)
start _ = return ()
-}
