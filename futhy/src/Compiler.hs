module Compiler where
import Types
import Utils

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
biop :: BilOp -> Arity -> Arity -> String
biop b a1 a2 =  let base = case b of
                            ScalarProd -> "scalarprod"
                            TensorProd -> "tensorprod"
                            MatrixMult -> "matrixmult"
                            DotProd -> "dotprod"
                            Mult -> "mult"
                            Outer -> "outer"
                in base <> "_" <> show(ua a1) <> "_" <> show(ua a2)
-- TODO: since there is no polymorphism in futhark, make the implementation either dynamic or depend on the arity of the arguments?

-- int used to make function name unique
-- string is the partially applied function (without parens)
gen_sloc :: Int -> String -> Arity -> (Int, String, Arity)
gen_sloc c futfun a = (c+1, "let FUN" <> show(c) <> " = (" <> futfun <> ")" <> nl, a)

-- receives an LFUN and a counter and generates any necessary subprograms, and then combines all of it to the new loc (preceded by any subprograms) and returns it with the new counter
-- the counter the number of total funs defined at a given point and preceding it, that are necessary to make the current one evaluable

-- every lfun has a type (t1 -> t2), where each of t1 and t2 are some arity of Vals
-- id takes a1 and returns a1 - that is, the arity/type is the same
-- dup takes a1 and returns (a1, a1)
-- scale takes 0 (a float) and a1 and returns a1, but is defined with 0 already and just needs to be applied to a1
-- comp takes a1, applies (a1 -> a2), and then (a2 -> a3), and returns a3 arity
-- a2 from the first and second must match, as must a1 and a1 from the first

lfun :: LFun -> Int -> Arity -> (Int, String, Arity) -- (new uid_count, Arity of lfun-input, new subprogram)
lfun linfun c1 a1 = case linfun of
  Id -> gen_sloc c1 "id" a1
  Dup -> gen_sloc c1 "dupe" (P a1 a1)
  Comp lf2 lf1 -> let (c2, subprog1, a2) = lfun lf1 c1 a1 in
                  let (c3, subprog2, a3) = lfun lf2 c2 a2 in
                  let (c4, nloc, a4) = gen_sloc c3 ("comp" <> " FUN" <> show(c3-1) <> " FUN" <> show(c2-1)) a3
                  in (c4, subprog2 <> subprog1 <> nloc, a4)
  Para lf2 lf1 ->
    case a1 of
      (P a3 a2) ->
        let (c2, subprog1, a4) = lfun lf1 c1 a2 in
        let (c3, subprog2, a5) = lfun lf2 c2 a3 in
        let (c4, nloc, a6) = gen_sloc c3 ("para" <> " FUN" <> show(c3-1) <> " FUN" <> show(c2-1)) (P a5 a4)
        in (c4, subprog2 <> subprog1 <> nloc, a6)
      _ -> undefined --ERROR, argument to para must be a Pair of Vals
  LSec v b -> gen_sloc c1 (biop b (val_arity v) a1 <> " " <> val v) a1
  RSec b v -> gen_sloc c1 (val v <> " " <> biop b (val_arity v) a1) a1
  Scale rn -> lfun (LSec (Scalar rn) Outer) c1 a1
  Zero -> lfun (Scale 0) c1 a1 --redirection
  _ -> undefined -- TODO: IMPLEMENT THE REST

  -- TODO: We need a desugar stage before compilation...


-- compile a whole program
program :: LFun -> Val -> String
program lf v = let (count, prog, _) = lfun lf 1 (val_arity v)
               in imports <> arg v <> prog <> nl <> entry count
