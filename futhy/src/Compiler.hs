module Compiler where
import Types

-- TODO: clean the whole thing.
-- reminder that <> is prettier than ++

nl :: String
nl = "\n"

datatype :: String
datatype = "f32"

imports :: String
imports = "open import \"lmaplib\"" <> nl <> nl

val :: Val -> String -- literal
val v = case v of
  Scalar sc -> show(sc) <> datatype -- this ensures that Futhark gives us what we want
  Pair v1 v2 -> "(" <> (val v1) <> "," <> (val v2) <> ")"
  Tensor ls -> "[" <> (val $ head ls) <> (concatMap (\w -> ", " <> val w) (tail ls)) <> "]"

arg :: Val -> String
arg v = "let ARG = " <> (val v) <> nl <> nl

entry :: Int -> String
entry count = "entry main = FUN" <> show(count-1) <> " ARG"

-- TODO: implement these in the futhark library
-- please let the biop data types have the same name as the lib func
-- just the spelling, I dont care about the casing
biop :: BilOp -> String
biop b = case b of
  ScalarProd -> "scalarprod"
  TensorProd -> "tensorprod"
  MatrixMult -> "matrixmult"
  DotProd -> "dotprod"
  Mult -> "mult"
  Outer -> "outer"

-- int used to make function name unique
-- string is the partially applied function (without parens)
gen_sloc :: Int -> String -> (Int, String)
gen_sloc c futfun = (c+1, "let FUN" <> show(c) <> " = (" <> futfun <> ")" <> nl)

-- receives an LFUN and a counter and generates any necessary subprograms, and then combines all of it to the new loc (preceded by any subprograms) and returns it with the new counter
-- the counter the number of total funs defined at a given point and preceding it, that are necessary to make the current one evaluable
lfun :: LFun -> Int -> (Int, String) -- (new uid_count, new subprogram)
lfun linfun c = case linfun of
  Scale rn -> gen_sloc c ("sv " <> show(rn))
  Dup -> gen_sloc c "dupe"
  Id -> gen_sloc c "id"
  Comp lf1 lf2 -> let (c2, subprog) = lfun lf1 c in
                  let (c3, subprog2) = lfun lf2 c2 in
                  let (c4, nloc) = gen_sloc c3 ("comp" <> " FUN" <> show(c2-1) <> " FUN" <> show(c3-1))
                  in (c4, subprog <> subprog2 <> nloc)
  Para lf1 lf2 -> let (c2, subprog) = lfun lf1 c in
                  let (c3, subprog2) = lfun lf2 c2 in
                  let (c4, nloc) = gen_sloc c3 ("para" <> " FUN" <> show(c2-1) <> " FUN" <> show(c3-1))
                  in (c4, subprog <> subprog2 <> nloc)
  LSec v b -> gen_sloc c (val v <> " " <> biop b)
  RSec b v -> gen_sloc c (biop b <> " " <> val v)
  Zero -> lfun (Scale 0) c --redirection
  _ -> undefined -- TODO: IMPLEMENT THE REST

-- compile a whole program
program :: LFun -> Val -> String
program lf v = let (count, prog) = lfun lf 1
               in imports <> arg v <> prog <> nl <> entry count
