module Main
  ( computeDerivative
  , main
  ) where

--import System.Environment (getArgs)
--import System.IO (getContents)
import Types
import Compiler
--import Optimizer
import Executor hiding (main)
import Options.Applicative
import ArgParser

-- Load as module
computeDerivative :: LFun -> Val -> DerivativeComputation ExecutionResult
computeDerivative lfun v =
  let futpgm = program lfun v
  in runStr futpgm C

-- Read from `stdin`, then print to `stdout`.
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (cliparser <**> helper)
      ( fullDesc
     <> progDesc "tad is a virtual coprocessor to speed up derivative computations for autodiff."
     <> header "tad - a virtual coprocessor to compute linear functions."
     <> footer "Thesis project by Ulrik Elmelund & Einar Rasmussen." )

{-
parseInput :: [String] -> (LFun, Val)
parseInput args = undefined
do
  args <- getArgs
  if not $ null args then let (lfun, val) = parseInput args else --listen stdin
  rv <- computeDerivative lfun val
  case rv
  print rv
-}
