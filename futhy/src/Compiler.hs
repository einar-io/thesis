module Compiler where
import Types
import Utils

nl :: String
nl = "\n"

datatype :: String
datatype = "f32"

imports :: String
imports = "open import \"lmaplib\"" <> nl <> nl

val :: Val -> String
val v = case v of
  Scalar sc -> show(sc) <> datatype -- so Futhark can disambiguate
  Pair v1 v2 -> "(" <> (val v1) <> "," <> (val v2) <> ")"
  Tensor ls -> "[" <> (val $ head ls) <> (concatMap (\w -> ", " <> val w) (tail ls)) <> "]"

spacefun :: String
spacefun = " fun"

arg :: Val -> String
arg v = "let arg = " <> (val v) <> nl <> nl

entry :: Int -> String
entry count = "entry main =" <> spacefun <> show(count-1) <> " arg" <> nl

biop :: BilOp -> Arity -> Arity -> String
biop b a1 a2 =  let base = case b of
                            ScalarProd -> "scalarprod"
                            TensorProd -> "tensorprod"
                            MatrixMult -> "matrixmult"
                            DotProd -> "dotprod"
                            Mult -> "mult"
                            Outer -> "outer"
                in base <> arityext a1 a2

arityext :: Arity -> Arity -> String
arityext a1 a2 = "_" <> show(ua a1) <> "_" <> show(ua a2)


type Program = String
type Count = Int

type CState = (Program, Arity, Count)
newtype Compiler a = Co {runCo :: CState -> (a, CState)}
instance Functor Compiler where
instance Applicative Compiler
instance Monad Compiler where
  return a = Co (\cs -> (a, cs))
  m >>= f = Co (\cs0 -> let (a,cs1) = runCo m cs0
                        in runCo (f a) cs1)

initComp :: CState -> Compiler ()
initComp cs = Co (\_ -> ((), cs))

getProgr :: Compiler Program
getProgr = Co (\cs@(prog, _, _) -> (prog, cs))

getCountArity :: Compiler (Count, Arity)
getCountArity = Co (\cs@(_, arit, cnt) -> ((cnt, arit), cs))

finishProg :: Compiler ()
finishProg =
  Co (\(p, r, c) ->
    let new_loc = "entry main =" <> spacefun <> show(c-1) <> " arg"
    in ((), (p <> new_loc, r, c)))

locM :: Arity -> Program -> Compiler ()
locM r fun =
  Co (\(p0, _, c0) ->
      let new_loc = "let" <> spacefun <> show(c0) <> " = (" <> fun <> ")" <> nl
      in ((), (p0 <> new_loc, r, c0+1)))

-- to monadically compile a function, give it the function and the arity (type) of the input to that function
-- the arity of the output of the function then depends on the input and the function itself
lfunM :: LFun -> Arity -> Compiler ()
lfunM linfun a1 = case linfun of
  Id -> locM a1 "id"
  Dup -> locM (P a1 a1) "dupe"
  Comp lf2 lf1 -> do
                    lfunM lf1 a1
                    (c2, a2) <- getCountArity
                    lfunM lf2 a2
                    (c3, a3) <- getCountArity
                    locM a3 ("comp" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1))
  Para lf2 lf1 ->
    case a1 of
      P a3 a2 -> do
                   lfunM lf1 a2
                   (c2, a4) <- getCountArity
                   lfunM lf2 a3
                   (c3, a5) <- getCountArity
                   locM (P a5 a4) ("para" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1))
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> do locM a1 (biop b (val_arity v) a1 <> " " <> val v)
  RSec b v -> do locM a1 (val v <> " " <> biop b (val_arity v) a1)
  Scale rn -> do lfunM (LSec (Scalar rn) Outer) a1
  Zero -> do lfunM (Scale 0) a1
  Prj i j -> case (i,j,a1) of
                (2, 1, P a3 _) -> do locM a3 "fst"
                (2, 2, P _ a2) -> do locM a2 "snd"
                _ -> undefined
  Lplus lf2 lf1 -> do lfunM (Para lf2 lf1) a1
                      (c2, a4) <- getCountArity
                      case a4 of
                        (P a3 a2) -> do locM a4 ("plus" <> arityext a3 a2 <> " " <> "fun" <> show(c2-1))
                        _ -> undefined
  Red _ -> undefined
  Add _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined

programM :: LFun -> Val -> Compiler Program
programM lf v = let arit = val_arity v in
                let initial = (imports <> arg v, arit, 1) in
                let act = do initComp initial
                             lfunM lf arit
                             finishProg
                             getProgr
                in act

evalCompiler :: Compiler Program -> CState -> Program
evalCompiler act = fst . runCo act

program :: LFun -> Val -> Program
program lf v = let arit = val_arity v in
               let act = programM lf v in
               let initial = (imports <> arg v, arit, 1) in
               evalCompiler act initial

-------------Compiler a = Co {runCo :: CState -> (a, CState)}

gen_sloc :: Count -> Program -> Arity -> (Count, Program, Arity)
gen_sloc c futfun a = (c+1, "let" <> spacefun <> show(c) <> " = (" <> futfun <> ")" <> nl, a)
