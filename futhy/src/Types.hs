{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DeriveAnyClass, DerivingStrategies, StandaloneDeriving #-}

module Types where

import GHC.Generics (Generic)
import Control.Monad.Reader
import Control.Monad.Except
import Control.DeepSeq
import GHC.IO.Exception( ExitCode ( ExitFailure ) )
import Data.List (intercalate)
import Flow

type RealNumber = Double
type Index = Int

data Val
  = Scalar RealNumber
  | Zero
  | Dummy
  | Vector [Val] -- Vector [Val]
  | Pair Val Val
  | SparseVector [(Index, Val)]
  deriving
    ( Eq
    , Generic
    , NFData
    )

instance Num Val where
 (Scalar n1) + (Scalar n2) = Scalar (n1 + n2)
 (Vector vs1) + (Vector vs2) = Vector (zipWith (+) vs1 vs2)
 Zero + v = v
 v + Zero = v
 _ + _ = undefined
 (Scalar n1) * (Scalar n2) = Scalar (n1 * n2)
 (Vector vs1) * (Vector vs2) = Vector (zipWith (*) vs1 vs2)
 Zero * _ = Zero
 _ * Zero = Zero
 _ * _ = undefined
 negate (Scalar n) = Scalar (-n)
 negate (Vector vs) = Vector (map negate vs)
 negate (SparseVector pivs) = SparseVector $ map (\(idx, v) -> (idx, negate v)) pivs
 negate Zero = Zero
 negate (Pair l r) = Pair (negate l) (negate r)
 negate _ = undefined
 abs (Scalar n) = Scalar (abs n)
 abs _ = undefined
 signum (Scalar n) = Scalar (signum n)
 signum Zero = 0
 signum _ = undefined
 fromInteger i = Scalar (fromInteger i)

instance Show Val where
  show (Scalar sc)  = if sc >= 0.0
                      then show sc <> "f32"
                      else "(" <> show sc <> "f32" <> ")"
  show (Pair v1 v2) = "(" <> show v1 <> ", " <> show v2 <> ")"
  show (Vector ls)  = "["
                        <> ( ls
                             |> map show
                             |> intercalate ", "
                           )
                        <> "]"
  show Zero  = show $ Scalar 0
  show Dummy = "f32.nan"
  show _ = undefined

stdinShow :: Val -> String
stdinShow (Scalar sc)  = " " <> show sc <> "f32 "
stdinShow Dummy        = " f32.nan "
stdinShow (Pair v2 v1) = stdinShow v2 <> " " <> stdinShow v1
stdinShow (Vector ls)  = " [" <> ( ls
                                 |> map stdinShow
                                 |> intercalate ", "
                                 ) <> "] "
stdinShow v = show v

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
  Dummy -> Atom 0
  Vector (Pair _ _: _) -> error "illegal Vector of pairs!"
  Vector (h:_) -> let (Atom i) = getArity h
                  in Atom (i+1)
  Pair v1 v2 -> APair (getArity v1) (getArity v2)
  Vector [] -> error "Arity-get failed: empty Vector not allowed"
  _ -> error $ "Arity not implemented for " <> show v

-- These are listed as linear map expressions
-- https://github.com/diku-dk/caddie/blob/master/src/lin.sig
-- [POPL, p. 21]
data LFun
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
  | InjFst
  | InjSnd
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
  | MatVecProd
  | VecMatProd
  | Outer
  | LossFunction
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

type Runs = Int

data Env = Env
  { fp   :: FilePath
  , ds   :: Dataset
  , be   :: Backend
  , runs :: Runs
  }

isExitFailure :: ExitCode -> Bool
isExitFailure (ExitFailure _) = True
isExitFailure _               = False

-- The data contained in the Right constructur of ExceptT trans:
type Stdin  = String
type Stdout = String
type CommandOutput = (ExitCode, Stdout, Stdin)
type JSON = String

data Log = Log
  { exitcode :: ExitCode
  , stdout   :: Stdout
  , stdin    :: Stdin
  , json     :: JSON
  } deriving (Show, Eq, Generic, NFData)

data FailedStep
  = CompilationError
  | ExecutionError
  | BenchmarkError
  | DatagenError
  deriving (Show, Eq, Generic, NFData)

data Failure = CommandFailure FailedStep CommandOutput
  deriving (Show, Eq, Generic, NFData)


--newtype Result = CommandResult Log
newtype Result = Result { getLog :: Log }
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

type CommandExecution a = Either Failure a

-- Command evaluation monad.
type Cmd a = ReaderT Env (ExceptT Failure IO) a
newtype Command a = Command { runCmd :: Cmd a }
  deriving newtype
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

type Program = String
type Count = Int
type CState = (Program, Arity, Count)


--type Bench = FilePath -> Backend -> Runs -> LFun -> Val -> IO (CommandExecution Result)
type Bench = Backend -> Runs -> Int -> IO (CommandExecution Result)
type Series = [Double]


type OOMs = (Int, Int)

type Dataset = FilePath
--type PlotData = (FilePath, [Int], [Series])
type PlotData = (String, String, ([Int], Series))
