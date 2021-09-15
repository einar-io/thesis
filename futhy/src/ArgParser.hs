module ArgParser where

import Options.Applicative
--import Data.Semigroup ((<>))

data Opts = Opts
  { input :: Input
  , output :: Output
  , backend :: String
  , quiet :: Bool
  , version :: Bool
  , license :: Bool } 

data Input
  = InFile FilePath

data Output
  = OutFile FilePath

inputP :: Parser Input
inputP = 
  InFile <$> 
      strOption
      ( long "input"
      <> short 'i'
      <> value "/dev/stdin"
      <> help "INPUT file containing LFUN program in point-free notation."
      <> metavar "FILE" )

outputP :: Parser Output
outputP = 
  OutFile <$> 
      strOption
      ( long "output"
      <> short 'o'
      <> value "/dev/stdout"
      <> help "OUTPUT file to write the result of the derivative computation."
      <> metavar "FILE" )

cliparser :: Parser Opts
cliparser = Opts 
  <$> inputP
  <*> outputP
  <*> strOption
      ( long "backend"
      <> short 'b'
      <> help "Choose Futhark backend.  BACKEND can be c, opencl or cuda.  The default is c."
      <> value "c"
      <> metavar "BACKEND" )
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

