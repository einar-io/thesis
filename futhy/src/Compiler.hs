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

getLastCountArity :: Compiler (Count, Arity)
getLastCountArity = Co (\cs@(_, arit, cnt) -> ((cnt-1, arit), cs))

-- Lines Of Code
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
                     (c2, a2) <- getLastCountArity
                     lfunM lf2 a2
                     (c3, a3) <- getLastCountArity
                     locM a3 ("comp" <> spacefun c3 <> spacefun c2)
  Para lf2 lf1 ->
    case a1 of
      P a3 a2 -> do lfunM lf1 a2
                    (c2, a4) <- getLastCountArity
                    lfunM lf2 a3
                    (c3, a5) <- getLastCountArity
                    locM (P a5 a4) ("para" <> spacefun c3 <> spacefun c2)
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> do locM a1 (biop b (val_arity v) a1 <> " " <> show v)
  RSec b v -> do locM a1 (show v <> " " <> biop b (val_arity v) a1)
  Scale rn -> do lfunM (LSec (Scalar rn) Outer) a1
  KZero -> do lfunM (Scale 0) a1
  Prj i j -> case (i,j,a1) of
                (2, 1, P a3 _) -> do locM a3 "fst"
                (2, 2, P _ a2) -> do locM a2 "snd"
                _ -> undefined
  Lplus lf2 lf1 -> do lfunM (Para lf2 lf1) a1
                      (c2, a4) <- getLastCountArity
                      case a4 of
                        (P a3 a2) -> do locM a4 ("plus" <> arityext a3 a2 <> spacefun c2)
                        _ -> undefined
  Add ->
    case a1 of
      P (Atom 0) (Atom 0) -> locM (Atom 0) "add_0_0"
      P a2 _ -> undefined -- locM (Atom a2) ("add" <> spacefun (c3-1) <> spacefun (c2-1))
  Red _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined

typeDeclared :: Arity -> String
typeDeclared a = case a of
  Atom 0 -> "f32"
  Atom n -> "[]" <> (typeDeclared $ Atom $ n-1)
  P a1 a2 -> "(" <> typeDeclared a1 <> ", " <> typeDeclared a2 <> ")"


finishProg :: Arity -> Compiler ()
finishProg a =
  Co (\(p, r, c) ->
    let new_loc = "entry main (input: " <>  typeDeclared a <> ") =" <> spacefun (c-1) <> " input"
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
                                      finishProg arit
                                      getProgr
                         in (fst . runCo act) initial
