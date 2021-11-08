{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Control.Monad.Reader
import Control.Monad.Except
import Control.DeepSeq
import GHC.IO.Exception( ExitCode ( ExitFailure ) )
import Data.List (intercalate)
import Flow
import Data.Time.Clock (UTCTime)

type RealNumber = Double
type Index = Int

data Val
  = Scalar RealNumber
  | Zero
  | Tensor [Val]
  | Pair Val Val
  | SparseTensor [(Index, Val)]
  deriving (Eq)


instance Num Val where
 (Scalar n1) + (Scalar n2) = Scalar (n1 + n2)
 (Tensor vs1) + (Tensor vs2) = Tensor (zipWith (+) vs1 vs2)
 Zero + v = v
 v + Zero = v
 (_) + (_) = undefined
 (Scalar n1) * (Scalar n2) = Scalar (n1 * n2)
 (Tensor vs1) * (Tensor vs2) = Tensor (zipWith (*) vs1 vs2)
 Zero * _ = Zero
 _ * Zero = Zero
 (_) * (_) = undefined
 negate (Scalar n) = Scalar (-n)
 negate (Tensor vs) = Tensor (map negate vs)
 negate (SparseTensor pivs) = SparseTensor $ map (\(idx, v) -> (idx, negate v)) pivs
 negate Zero = Zero
 negate (Pair l r) = Pair (negate l) (negate r)
 abs (Scalar n) = Scalar (abs n)
 abs (_) = undefined
 signum (Scalar n) = Scalar (signum n)
 signum Zero = 0
 signum (_) = undefined
 fromInteger i = Scalar (fromInteger i)

instance NFData Val where
 rnf a = ()

instance Show Val where
  show v = case v of
    Scalar sc -> if sc >= 0.0 then show sc <> "f32" else "(" <> show sc <> "f32" <> ")"
    Pair v1 v2 -> "(" <> show v1 <> ", " <> show v2 <> ")"
    Tensor ls ->
      --"DenseTensor ["
      "["
      <> ( ls
           |> map show
           |> intercalate ", "
         )
      <> "]"
    Zero -> show $ Scalar 0
    SparseTensor _ ->
      "[0.0f32, 0.0f32]"

stdinShow :: Val -> String
stdinShow v = case v of
  Scalar sc  -> " " <> show sc <> "f32 "
  Pair v2 v1 -> stdinShow v2 <> " " <> stdinShow v1
  Tensor ls  -> "[" <> ( ls
                         |> map stdinShow
                         |> intercalate ", "
                        ) <> "]"
  _ -> show v


data Arity
  = Atom Int
  | APair Arity Arity
  deriving (Eq)

instance Show Arity where
  show a = case a of
    Atom i -> show i
    APair a3 a2 -> "(" <> show a3 <> ", " <> show a2 <> ")"

getArity :: Val -> Arity
getArity v = case v of
  Scalar _ -> Atom 0
  Zero -> Atom 0
  Tensor (Pair _ _: _) -> error "illegal tensor of pairs!"
  Tensor (h:_) -> let (Atom i) = getArity h
                  in Atom (i+1)
  Pair v1 v2 -> APair (getArity v1) (getArity v2)
  Tensor [] -> error "Arity-get failed: empty tensor not allowed"
  _ -> error $ "Arity not implemented for " <> show v

-- These are listed as linear map expressions
-- https://github.com/diku-dk/caddie/blob/master/src/lin.sig
-- [POPL, p. 21]
data LFun -- expr
  -- Arity preserving
  = Id
  | KZero
  | Scale RealNumber
  | Neg
  | LSec Val BilOp
  | RSec BilOp Val
  | LMap LFun
  | Zip [LFun]
  | Para LFun LFun -- more descriptive than Oplus
  | Comp LFun LFun
  -- Arity changing
  | Dup
  | Prj Int Int
  | Fst
  | Snd
  | Add
  | Lplus LFun LFun -- lifted addition
  | Red Rel
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

data Env = Env
  { fp :: FilePath
  , be :: Backend
  }


isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure _               = False


-- The data contained in the Right constructur of ExceptT trans:
-- Maybe rename this to CmdResult
type Stdin  = String
type Stdout = String
type CommandOutput = (ExitCode, Stdout, Stdin)

type TimeStamp = UTCTime

data Log = Log
  { exitcode :: ExitCode
  , stdout   :: Stdout
  , stdin    :: Stdin
  , begin    :: TimeStamp
  , finish   :: TimeStamp
  }

data FailedStep
  = CompilationError
  | ExecutionError
  deriving (Show, Eq)

newtype Result = CommandResult Log
--  deriving (Show, Eq)

data Failure = CommandFailure FailedStep CommandOutput
  deriving (Show, Eq)

type CommandExecution a = Either Failure a

-- Command evaluation monad.
type Cmd a = ReaderT Env (ExceptT Failure IO) a
newtype Command a = Command { runCmd :: Cmd a }
  deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadReader Env
  , MonadError Failure
  )

execCmd :: Command a -> Env -> IO (CommandExecution a)
execCmd cmd env = runExceptT $ runReaderT (runCmd cmd) env


type InterpretorError    = String
type InterpretorResult   = Val
type InterpretorOutput a = Either InterpretorError a

