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
  pure = return
  (<*>) a b = do x <- a
                 x <$> b

instance Monad Compiler where
  return a = Co (\cs -> (a, cs))
  m >>= f = Co (\cs0 -> let (a,cs1) = runCo m cs0
                        in runCo (f a) cs1)

biop :: BilOp -> Arity -> Arity -> String
biop b a1 a2 =  let base = case b of
                            ScalarProd -> "scalarprod"
                            TensorProd -> "tensorprod"
                            MatrixMult -> "matrixmult"
                            DotProd -> "dotprod"
                            Mult -> "mult"
                            Outer -> "outer"
                in base <> arityAnnotation a1 a2

arityAnnotation :: Arity -> Arity -> String
arityAnnotation a1 a2 = "_" <> show a1 <> "_" <> show a2

getLastCountAndArity :: Compiler (Count, Arity)
getLastCountAndArity = Co (\cs@(_, arit, cnt) -> ((cnt-1, arit), cs))

-- Lines Of Code
genLineOfCode :: Arity -> Program -> Compiler ()
genLineOfCode r fun =
  Co (\(p, _, c) ->
      let new_loc = "let" <> " fun" <> show c <> " = (" <> fun <> ")" <> "\n"
      in ((), (p <> new_loc, r, c+1)))

compileLFun :: LFun -> Arity -> Compiler ()
compileLFun linfun a1 = case linfun of
  Id -> genLineOfCode a1 "id"
  Dup -> genLineOfCode (APair a1 a1) "dupe"
  Comp lf2 lf1 -> do compileLFun lf1 a1
                     (c2, a2) <- getLastCountAndArity
                     compileLFun lf2 a2
                     (c3, a3) <- getLastCountAndArity
                     genLineOfCode a3 ("comp" <> " fun" <> show c3 <> " fun" <> show c2)
  Para lf2 lf1 ->
    case a1 of
      APair a3 a2 -> do compileLFun lf1 a2
                        (c4, a4) <- getLastCountAndArity
                        compileLFun lf2 a3
                        (c5, a5) <- getLastCountAndArity
                        genLineOfCode (APair a5 a4) ("para" <> " fun" <> show c5 <> " fun" <> show c4)
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> genLineOfCode a1 (biop b (getArity v) a1 <> " " <> show v)
  RSec b v -> genLineOfCode a1 (show v <> " " <> biop b (getArity v) a1)
  Scale rn -> compileLFun (LSec (Scalar rn) Outer) a1
  KZero -> compileLFun (Scale 0) a1
  Prj i j -> case (i,j,a1) of
                (2, 1, APair a3 _) -> genLineOfCode a3 "fst"
                (2, 2, APair _ a2) -> genLineOfCode a2 "snd"
                _ -> undefined
  Lplus lf3 lf2 -> do compileLFun lf2 a1
                      (c2, a2) <- getLastCountAndArity
                      compileLFun lf3 a1
                      (c3, a3) <- getLastCountAndArity
                      case (a2, a3) of
                        (Atom _, Atom _) -> genLineOfCode a2 ("lplus_" <> show a2 <> " fun" <> show c3 <> " fun" <> show c2)
                        _ -> undefined

  Add ->
    case a1 of
      APair (Atom 0) (Atom 0) -> genLineOfCode (Atom 0) "add_0_0"
      _ -> undefined -- genLineOfCode (Atom a2) ("add" <> " fun" <> show (c3-1) <> " fun" <> show (c2-1))
  Red _ -> undefined
  LMap _ -> undefined
  Zip _ -> undefined
  Neg -> undefined

typeDeclared :: Arity -> String
typeDeclared a = case a of
  Atom 0 -> "f32"
  Atom n -> "[]" <> typeDeclared (Atom $ n-1)
  APair a1 a2 -> "(" <> typeDeclared a1 <> ", " <> typeDeclared a2 <> ")"


finishProg :: Arity -> Compiler ()
finishProg a =
  Co (\(p, r, c) ->
    let new_loc = "entry main (input: " <>  typeDeclared a <> ") =" <> " fun" <> show (c-1) <> " input"
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
                                      compileLFun lf arit
                                      finishProg arit
                                      getProgr
                         in (fst . runCo act) initial
