module Main
  ( computeDerivative
  , main
  ) where

import System.Environment (getArgs)
import System.IO (getContents)
import Options.Applicative
import Data.Semigroup ((<>))
import Types
import Compiler
--import Optimizer
import Executor hiding (main)


data Opts = Opts
  { stdin :: Bool
  , backend :: String
  , input :: FilePath
  , output :: FilePath
  , quiet :: Bool
  , version :: Bool
  , license :: Bool } 



cliparser :: Parser Opts
cliparser = Opts
  <$> switch
      ( long ""
      <> help "Read from stdin." )
  <*> strOption
      ( long "backend"
      <> short 'b'
      <> help "Choose Futhark backend.  BACKEND is either c, opencl or cuda.  Defaults to c."
      <> metavar "BACKEND" )
  <*> strOption
      ( long "input"
      <> short 'i'
      <> help "INPUT file containing LFUN program in point-free notation."
      <> metavar "FILE" )
  <*> strOption
      ( long "output"
      <> short 'o'
      <> help "OUTPUT file to write the result of the computation." 
      <> metavar "FILE" )
  <*> switch
      ( long "quiet"
      <> short 'q'
      <> help "Do not print log statements to stderr." )
  <*> switch
      ( long "version"
      <> short 'V'
      <> help "Show version." )
  <*> switch
      ( long "license"
      <> short 'l'
      <> help "Show license." )
     







parseInput :: [String] -> (LFun, Val)
parseInput args = undefined

-- Load as module
computeDerivative :: LFun -> Val -> DerivativeComputation ExecutionResult
computeDerivative lfun val =
  let futpgm = program lfun val
  in runStr futpgm C

-- Read from `stdin`, then print to `stdout`.
main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (cliparser <**> helper)
      ( fullDesc
     <> progDesc "tad is a virtual coprocessor to speed up derivative computations for autodiff."
     <> header "hello - a test for optparse-applicative" )

greet :: Opts -> IO ()
greet (Opts True "c" input output False False False ) = putStrLn $ "Hello, " ++ input ++ "!"
greet _ = return ()

{-
do
  args <- getArgs
  if not $ null args then let (lfun, val) = parseInput args else --listen stdin
  rv <- computeDerivative lfun val
  case rv
  print rv
-}
