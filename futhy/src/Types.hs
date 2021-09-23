{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
import GHC.IO.Exception (ExitCode)
import Data.List (intercalate)
import Flow

type RealNumber = Float


data Val
  = Scalar RealNumber
  | Zero
  | Tensor [Val]
  | Pair Val Val
  | SparseTensor [(Index, Val)]
  | VList [Val]
  deriving (Eq)


instance Num Val where
 (Scalar n1) + (Scalar n2) = Scalar (n1 + n2)
 (Tensor vs1) + (Tensor vs2) = Tensor (zipWith (+) vs1 vs2)
 Zero + v = v
 v + Zero = v
 (Scalar n1) * (Scalar n2) = Scalar (n1 * n2)
 (Tensor vs1) * (Tensor vs2) = Tensor (zipWith (*) vs1 vs2)
 Zero * _ = Zero
 _ * Zero = Zero
 negate (Scalar n) = Scalar (-n)
 negate (Tensor vs) = Tensor (map negate vs)
 negate (SparseTensor pivs) = SparseTensor $ map (\(idx, v) -> (idx, negate v)) pivs
 negate Zero = Zero
 negate (Pair l r) = Pair (negate l) (negate r)
 abs (Scalar n) = Scalar (abs n)
 signum (Scalar n) = Scalar (signum n)
 signum Zero = 0
 fromInteger i = Scalar (fromInteger i)

instance Show Val where
  show v = case v of
    Scalar sc -> if sc >= 0.0 then show sc <> "f32" else "(" <> show sc <> "f32" <> ")"
    Pair v1 v2 -> "(" <> show v1 <> ", " <> show v2 <> ")"
    --Tensor ls -> "[" <> show (head ls) <> concatMap (\w -> ", " <> show w) (tail ls) <> "]"
    Tensor ls ->
      --"DenseTensor ["
      "["
      ++ ( ls
           |> map show
           |> intercalate ", "
         )
      ++ "]"
    Zero -> show $ Scalar 0
    SparseTensor vs ->
      "SparseTensor <["
      ++ ( vs
           |> map (\(i, v) -> "(" ++ show i ++ ": " ++ show v ++ ")")
           |> intercalate ", "
         )
      ++ "]>"

-- These are listed as linear map expressions
-- https://github.com/diku-dk/caddie/blob/master/src/lin.sig
-- [POPL, p. 21]
data LFun -- expr
  = Id
  | Dup
  | KZero
  | Scale RealNumber
  | LSec Val BilOp
  | RSec BilOp Val
  | Para LFun LFun -- more descriptive than Oplus
  | Comp LFun LFun
  | Prj Int Int
  | Lplus LFun LFun -- lifted addition
  | Red Rel
  | Add
  | LMap LFun
  | Zip [LFun]
  | Neg
  deriving (Show, Eq)

data Rel
  = List [(Int, Int)]
  | Func RelFun
  deriving (Show, Eq)

newtype RelFun
  = Const Int
  deriving (Show, Eq)

-- These are bilinear operators
-- listed on [POPL, p. 20]
data BilOp
  = MatrixMult
  | DotProd
  | Outer
  deriving (Show, Eq)

type Derivative = Val

-- error types!
newtype Error
  = Something String
  deriving (Show, Eq)

type Filepath   = String
type FutPgmFile = String
type FutPgmStr  = String
type FutPgmExec = String
type StdInArg = String

data Backend
    = C
    | OPENCL
    | CUDA
    deriving (Read)

instance Show Backend where
  show b = case b of
    C -> "c"
    OPENCL -> "opencl"
    CUDA -> "cuda"


-- The data contained in the Right constructur of ExceptT trans:
-- Maybe rename this to CmdResult
type Stdin  = String
type Stdout = String
type CommandOutput = (ExitCode, Stdout, Stdin)

-- Error types possible in the Left constructor of ExceptT trans.
data CommandError
  = CompilationError CommandOutput
  | ExecutionError CommandOutput
  | InterpretorError String
  deriving (Show, Eq)

-- Result types possible in the Right constructor of ExceptT trans.
data CommandResult
  = RawFuthark CommandOutput
  | Output CommandOutput
  | StructuredFuthark Val
  | InterpretorResult Val
  deriving (Show, Eq)


{-
type InterpretorError  = String
type InterpretorResult = Val
type CommandError   = (ErrorType, CommandType)
type CommandResult2 = CommandOutput
Right (ec, stdout, stdin)
Left  (_, (ec, stdout, stdout))
-}



data Env = Env {
    fp :: FilePath
  , be :: Backend
  }

-- Command evaluation monad.
type Cmd a = ReaderT Env (ExceptT CommandError IO) a
newtype Command a = Command { runCmd :: Cmd a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Env
  , MonadError CommandError
  )

type DerivativeComputation a = Either CommandError a

execCmd :: Command a -> Env -> IO (DerivativeComputation a)
execCmd cmd env = runExceptT $ runReaderT (runCmd cmd) env


type InterpretorError = String

type InterpretorOutput a = Either InterpretorError a

type Index = Int
