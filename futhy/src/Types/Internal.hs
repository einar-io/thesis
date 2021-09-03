module Types.Internal where


-- error types!
data Error
  = Something String
  deriving (Show, Eq)

type Filename = String
type FutFile = String
type FutStr = String
type FutExec = String
type CommandResult = (String, String, String)


data Backend
    = C
    | OpenCL
    | CUDA

instance Show Backend where
  show C      = "c"
  show CUDA   = "cuda"
  show OpenCL = "opencl"

