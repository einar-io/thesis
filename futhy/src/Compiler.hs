module Compiler where
import Types
import Utils
import Control.Monad.State.Lazy

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
entry count = "entry main =" <> spacefun <> show(count-1) <> " arg"

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

lfun :: LFun -> Int -> Arity -> (Int, String, Arity)
lfun linfun c1 a1 = case linfun of
  Id -> gen_sloc c1 "id" a1
  Dup -> gen_sloc c1 "dupe" (P a1 a1)
  Comp lf2 lf1 -> let (c2, subprog1, a2) = lfun lf1 c1 a1 in
                  let (c3, subprog2, a3) = lfun lf2 c2 a2 in
                  let (c4, nloc, a4) = gen_sloc c3 ("comp" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1)) a3
                  in (c4, subprog2 <> subprog1 <> nloc, a4)
  Para lf2 lf1 ->
    case a1 of
      P a3 a2 ->
        let (c2, subprog1, a4) = lfun lf1 c1 a2 in
        let (c3, subprog2, a5) = lfun lf2 c2 a3 in
        let (c4, nloc, a6) = gen_sloc c3 ("para" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1)) (P a5 a4)
        in (c4, subprog2 <> subprog1 <> nloc, a6)
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> gen_sloc c1 (biop b (val_arity v) a1 <> " " <> val v) a1
  RSec b v -> gen_sloc c1 (val v <> " " <> biop b (val_arity v) a1) a1
  Scale rn -> lfun (LSec (Scalar rn) Outer) c1 a1
  Zero -> lfun (Scale 0) c1 a1 --redirection
  Prj i j -> case (i,j,a1) of
                (2, 1, P a2 _) -> gen_sloc c1 "fst" a2
                (2, 2, P _ a2) -> gen_sloc c1 "snd" a2
                _ -> undefined
  Lplus lf2 lf1 -> let (c2, subprog, a4@(P a3 a2)) = lfun (Para lf2 lf1) c1 a1 in
                   let (c3, nloc, a5) = gen_sloc c2 ("plus" <> arityext a3 a2 <> " " <> "fun" <> show(c2-1)) a4
                   in (c3, subprog <> nloc, a5)
  Red _ -> undefined
  Add _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined


type Program = String
type Count = Int

type CState = (Program, Arity, Count)
newtype Compiler a = Co {runCo :: CState -> (a, CState)}
instance Functor Compiler
instance Applicative Compiler
instance Monad Compiler where
  return a = Co (\cs -> (a, cs))
  m >>= f = Co (\cs0 -> let (a,cs1) = runCo m cs0
                        in runCo (f a) cs1)

initComp :: CState -> Compiler ()
initComp cs = Co (\_ -> ((), cs))

getProgr :: Compiler Program
getProgr = Co (\cs@(prog, _, _) -> (prog, cs))

getArity :: Compiler Arity
getArity = Co (\cs@(_, arit, _) -> (arit, cs))

getCountArity :: Compiler (Count, Arity)
getCountArity = Co (\cs@(_, arit, cnt) -> ((cnt, arit), cs))

getCount :: Compiler Count
getCount = Co (\cs@(_, _, cnt) -> (cnt, cs))

finishProg :: Compiler ()
finishProg =
  Co (\(p, r, c) ->
    let new_loc = "entry main =" <> spacefun <> show(c) <> " arg"
    in ((), (p <> new_loc, r, c)))

locMArit :: Arity -> Program -> Compiler ()
locMArit r fun =
  Co (\(p0, _, c0) ->
      let new_loc = "let" <> spacefun <> show(c0) <> " = (" <> fun <> ")" <> nl
      in ((), (p0 <> new_loc, r, c0+1)))

locM :: Program -> Compiler ()
locM fun = do r <- getArity
              locMArit r fun

-- to monadically compile a function, give it the function and the arity (type) of the input to that function
-- the arity of the output of the function then depends on the input and the function itself
lfunM :: LFun -> Arity -> Compiler ()
lfunM linfun a1 = case linfun of
  Id -> locM "id"
  Dup -> locMArit (P a1 a1) "dupe"
  Comp lf2 lf1 -> do
                    lfunM lf1 a1
                    (c2, a2) <- getCountArity
                    lfunM lf2 a2
                    (c3, a3) <- getCountArity
                    locMArit a3 ("comp" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1))
  Para lf2 lf1 ->
    case a1 of
      P a3 a2 -> do
                   lfunM lf1 a2
                   (c2, a4) <- getCountArity
                   lfunM lf2 a3
                   (c3, a5) <- getCountArity
                   locMArit (P a5 a4) ("para" <> spacefun <> show(c3-1) <> spacefun <> show(c2-1))
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> do locMArit a1 (biop b (val_arity v) a1 <> " " <> val v)
  RSec b v -> do locMArit a1 (val v <> " " <> biop b (val_arity v) a1)
  Scale rn -> do lfunM (LSec (Scalar rn) Outer) a1
  Zero -> do lfunM (Scale 0) a1
  Prj i j -> case (i,j,a1) of
                (2, 1, P a3 _) -> do locMArit a3 "fst"
                (2, 2, P _ a2) -> do locMArit a2 "snd"
                _ -> undefined
  Lplus lf2 lf1 -> do lfunM (Para lf2 lf1) a1
                      a4 <- getArity
                      c2 <- getCount
                      case a4 of
                        (P a3 a2) -> do locMArit a4 ("plus" <> arityext a3 a2 <> " " <> "fun" <> show(c2-1))
                        _ -> undefined
  Red _ -> undefined
  Add _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined




programM :: LFun -> Val -> Compiler Program
programM lf v = do
                  let arit = val_arity v
                  let initial = (imports <> arg v, arit, 1)
                  --initComp initial
                  lfunM lf arit
                  finishProg
                  getProgr


-------------Compiler a = Co {runCo :: CState -> (a, CState)}

gen_sloc :: Count -> Program -> Arity -> (Count, Program, Arity)
gen_sloc c futfun a = (c+1, "let" <> spacefun <> show(c) <> " = (" <> futfun <> ")" <> nl, a)
