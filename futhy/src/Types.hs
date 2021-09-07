{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
--import Control.Monad.Trans.Except (ExceptT, runExceptT)
import GHC.IO.Exception (ExitCode)

type RealNumb = Float

data Val
  = Scalar RealNumb
  | Tensor [Val]
  | Pair Val Val
  deriving (Show, Eq)

-- These are listed as linear map expressions
-- [POPL, p. 21]
data LFun -- expr
  = Id
  | Dup
  | Zero
  | Scale RealNumb
  | LSec Val BilOp
  | RSec BilOp Val
  | Para LFun LFun -- more descriptive than Oplus
  | Comp LFun LFun
  | Prj Int Int
  | Lplus LFun LFun -- lifted addition
  | Red Rel
  | Add Int
  | LMap LFun
  | Zip [LFun]
  deriving (Show, Eq)

data Rel
  = List [(Int, Int)]
  | Func RelFun
  deriving (Show, Eq)

data RelFun
  = Const Int
  deriving (Show, Eq)

-- These are bilinear operators
-- listed on [POPL, p. 20]
data BilOp
  = ScalarProd
  | TensorProd
  | MatrixMult
  | DotProd
  | Mult
  | Outer
  deriving (Show, Eq)

type Derivative = Val

-- error types!
data Error
  = Something String
  deriving (Show, Eq)

type Filepath = String
type FutPgmFile = String
type FutPgmStr  = String
type FutPgmExec = String

data Backend
    = C
    | OpenCL
    | CUDA

instance Show Backend where
  show C      = "c"
  show CUDA   = "cuda"
  show OpenCL = "opencl"

-- Error types possible in the Left constructor of ExceptT trans.
data ExecutionError
  = CompilationError ExitCode
  | ExecutionError ExitCode
  deriving (Show, Eq)

-- ExitCode, Stdout, Stdin
type ExecutionResult = (ExitCode, String, String)

-- Result types possible in the Right constructor of ExceptT trans.
data ExecutorResult
  = Unparsed String
  | Parsed Val
  deriving (Show, Eq)

data Env = Env {
    fp :: FilePath
  , be :: Backend
  }

-- Command evaluation monad.
type Cmd a = ReaderT Env (ExceptT ExecutionError IO) a
newtype Command a = Command { runCmd :: Cmd a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Env
  , MonadError ExecutionError
  )

execCmd :: Command a -> Env -> IO (Either ExecutionError a)
execCmd cmd env = runExceptT $ runReaderT (runCmd cmd) env
