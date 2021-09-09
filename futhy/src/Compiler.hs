module Compiler where
import Types
import Utils

type Program = String
type Count = Int
type CState = (Program, Arity, Count)
newtype Compiler a = Co {runCo :: CState -> (a, CState)}
instance Functor Compiler where
  fmap f (Co g) = Co (\s0 -> let (a, s1) = g s0
                             in (f a, s1))

instance Applicative Compiler where
  pure a = return a
  (<*>) a b = do
                x <- a
                y <- b
                return (x y)

instance Monad Compiler where
  return a = Co (\cs -> (a, cs))
  m >>= f = Co (\cs0 -> let (a,cs1) = runCo m cs0
                        in runCo (f a) cs1)

val :: Val -> String
val v = case v of
  Scalar sc -> show(sc) <> "f32"
  Pair v1 v2 -> "(" <> (val v1) <> "," <> (val v2) <> ")"
  Tensor ls -> "[" <> (val $ head ls) <> (concatMap (\w -> ", " <> val w) (tail ls)) <> "]"

spacefun :: Int -> String
spacefun c = " fun" <> show(c)

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

getCountArity :: Compiler (Count, Arity)
getCountArity = Co (\cs@(_, arit, cnt) -> ((cnt, arit), cs))

locM :: Arity -> Program -> Compiler ()
locM r fun =
  Co (\(p, _, c) ->
      let new_loc = "let" <> spacefun c <> " = (" <> fun <> ")" <> "\n"
      in ((), (p <> new_loc, r, c+1)))

lfunM :: LFun -> Arity -> Compiler ()
lfunM linfun a1 = case linfun of
  Id -> locM a1 "id"
  Dup -> locM (P a1 a1) "dupe"
  Comp lf2 lf1 -> do lfunM lf1 a1
                     (c2, a2) <- getCountArity
                     lfunM lf2 a2
                     (c3, a3) <- getCountArity
                     locM a3 ("comp" <> spacefun (c3-1) <> spacefun (c2-1))
  Para lf2 lf1 ->
    case a1 of
      P a3 a2 -> do lfunM lf1 a2
                    (c2, a4) <- getCountArity
                    lfunM lf2 a3
                    (c3, a5) <- getCountArity
                    locM (P a5 a4) ("para" <> spacefun (c3-1) <> spacefun (c2-1))
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

finishProg :: Compiler ()
finishProg =
  Co (\(p, r, c) ->
    let new_loc = "entry main =" <> spacefun (c-1)
    in ((), (p <> new_loc, r, c)))

put :: CState -> Compiler ()
put cs = Co (\_ -> ((), cs))

getProgr :: Compiler Program
getProgr = Co (\cs@(prog, _, _) -> (prog, cs))

compileProgram :: LFun -> Arity -> Program
compileProgram lf arit = let initial_program =
                               "open import \"lmaplib\"\n\n" in
                         let initial = (initial_program, arit, 1) in
                         let act = do put initial
                                      lfunM lf arit
                                      finishProg
                                      getProgr
                         in (fst . runCo act) initial
