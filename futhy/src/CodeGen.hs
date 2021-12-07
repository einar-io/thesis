module CodeGen where
import Types
import Preprocesser

newtype CodeGen a = Co {runCo :: CState -> (a, CState)}
instance Functor CodeGen where
  fmap f (Co g) = Co (\s0 -> let (a, s1) = g s0
                             in (f a, s1))

instance Applicative CodeGen where
  pure = return
  (<*>) a b = do x <- a
                 x <$> b

instance Monad CodeGen where
  return a = Co (\cs -> (a, cs))
  m >>= f = Co (\cs0 -> let (a,cs1) = runCo m cs0
                        in runCo (f a) cs1)

biop :: BilOp -> Arity -> Arity -> String
biop b a1 a2 =  let base = case b of
                            MatrixMult -> "inner"
                            DotProd    -> "inner"
                            VecMatProd -> "inner"
                            MatVecProd -> "inner"
                            LossFunction-> "lossfunction"
                            Outer -> "outer"
                in base <> arityAnnotation a1 a2

arityAnnotation :: Arity -> Arity -> String
arityAnnotation a1 a2 = "_" <> show a1 <> "_" <> show a2

getLastFunIdAndArity :: CodeGen (String, Arity)
getLastFunIdAndArity = Co (\cs@(_, arit, cnt) -> ((" fun" <> show (cnt-1), arit), cs))

genLineOfCode :: Arity -> Program -> CodeGen ()
genLineOfCode r fun =
  Co (\(p, _, c) ->
      let new_loc = "let" <> " fun" <> show c <> " = (" <> fun <> ")" <> "\n"
      in ((), (p <> new_loc, r, c+1)))


getReduceResultDim :: [(Int, Int)] -> Int
getReduceResultDim ls = let (_, dst) = unzip ls in 1 + maximum dst


verifyZipToMap2Legality :: [LFunP] -> Bool
verifyZipToMap2Legality _ = True


codeGenLFun :: LFun -> Arity -> CodeGen ()
codeGenLFun linfun a1 = case (linfun, a1) of
  (Id, _)              -> genLineOfCode a1 "id"
  (Dup, _)             -> genLineOfCode (APair a1 a1) "dupe"
  (Fst, APair a3 _)    -> genLineOfCode a3 "fst"
  (Snd, APair _ a2)    -> genLineOfCode a2 "snd"
  (Add, APair a3 a2)   -> genLineOfCode a2 ("add_" <> show a3 <> "_" <> show a2)
  (Neg, _)             -> genLineOfCode a1 ("neg_" <> show a1)
  (Red (List _), Atom 0)  -> error "Red not meaningful for an Atom 0 argument"
  (Red (List ls), Atom n) -> genLineOfCode a1 ("reduce_" <> show n <> " " <> show ls <> " " <> show (getReduceResultDim ls))

  (LSec v b, _)        -> genLineOfCode a1 (biop b (getArity v) a1 <> " " <> show v)
  (RSec b v, _)        -> genLineOfCode (getArity v) ("flip " <> biop b a1 (getArity v) <> " " <> show v)
  (Comp lf3 lf2, _) -> do codeGenLFun lf2 a1
                          (id2, a2) <- getLastFunIdAndArity
                          codeGenLFun lf3 a2
                          (id3, a3) <- getLastFunIdAndArity
                          genLineOfCode a3 ("comp" <> id3 <> id2)
  (Para lf3 lf2, APair a3 a2) -> do codeGenLFun lf2 a2
                                    (id4, a4) <- getLastFunIdAndArity
                                    codeGenLFun lf3 a3
                                    (id5, a5) <- getLastFunIdAndArity
                                    genLineOfCode (APair a5 a4) ("para" <> id5 <> id4)
  (LMap _, Atom 0) -> error "LMap not meaningful for an Atom 0 argument"
  (LMap lf, Atom n) -> do codeGenLFun lf $ Atom $ n-1
                          (id2, _) <- getLastFunIdAndArity
                          genLineOfCode a1 ("map" <> id2)
  (Zip _, Atom 0)   -> error "zip not meaningful for an Atom 0 argument"
  (Zip lfs, Atom n) -> do let ((hf:_), vs@(hv:_)) = unzip $ map extractLFunConsts lfs
                          codeGenLFunP hf hv (Atom $ n-1)
                          (id2, _) <- getLastFunIdAndArity
                          -- TODO CALL verifyZipToMap2Legality
                          -- TODO CALL verifyZipToMap2Legality
                          -- TODO CALL verifyZipToMap2Legality
                          -- TODO CALL verifyZipToMap2Legality
                          genLineOfCode a1 ("unzipmap2" <> id2 <> " " <> show vs)

  (Zip _, _) -> error "illegal zip"

--- error section
  (Red (List _), _)  -> error "Meaningless arity given to Red."
  (Para _ _, _)      -> error "Meaningless arity given to Para."
  (Fst , _)          -> error "Meaningless arity given to Fst."
  (Snd , _)          -> error "Meaningless arity given to Snd."
  (Add , _)          -> error "Meaningless arity given to Add."
  (LMap _, _)        -> error "Meaningless arity given to LMap."
  -- desugared
  (KZero, _)         -> error "KZero should have been desugared!"
  (Scale _, _)       -> error "Scale should have been desugared!"
  (Prj _ _, _)       -> error "Prj should have been desugared!"
  (Lplus _ _, _)     -> error "Lplus should have been desugared!"
-- missing impl
  (Red _, _)         -> error "This relation not implemented in CodeGen"

-- the Val is the constant(s) needed to partially apply the LFunP and turn it into a unary LFun.
-- those that are already unary are given Zero as a dummy value, to be ignored.
-- those recursive contain the values of their children.

-- TODO: If flag set, visualize constant to right of function to effect partial application, somehow?
codeGenLFunP :: LFunP -> Val -> Arity -> CodeGen ()
codeGenLFunP lfp1 v1 a1 = case (lfp1, a1, v1) of
  (IdP,_, _)             -> genLineOfCode a1 "toss_dummy_const id"
  (DupP,_,_)             -> genLineOfCode (APair a1 a1) "toss_dummy_const dupe"
  (FstP, APair a3 _, _)  -> genLineOfCode a3 "toss_dummy_const fst"
  (SndP, APair _ a2, _)  -> genLineOfCode a2 "toss_dummy_const snd"
  (AddP, APair a3 a2, _) -> genLineOfCode a2 ("toss_dummy_const add_" <> show a3 <> "_" <> show a2)
  (NegP, _, _)           -> genLineOfCode a1 ("toss_dummy_const neg_" <> show a1)
  (RedP (List _), Atom 0, _)  -> error "Red not meaningful for an Atom 0 argument"
  (RedP (List ls), Atom n, _) -> genLineOfCode a1 ("toss_dummy_const (reduce_" <> show n <> " " <> show ls <> " " <> show (getReduceResultDim ls) <> ")")


  (LSecP op, _, v) -> genLineOfCode a1 $ "uncurry " <> (biop op (getArity v) a1)  -- <> " " <> show v)
  (RSecP op, _, v) -> genLineOfCode (getArity v) $ "uncurry " <> ("(flip " <> biop op a1 (getArity v)) <> ")" -- <> " " <> show v)

  (CompP lfp3 lfp2, _, Pair v3 v2) -> do codeGenLFunP lfp2 v2 a1
                                         (id2, a2) <- getLastFunIdAndArity
                                         codeGenLFunP lfp3 v3 a2
                                         (id3, a3) <- getLastFunIdAndArity
                                         genLineOfCode a3 ("pass_consts_comp" <> id3 <> id2)
  (ParaP lfp3 lfp2, APair a3 a2, Pair v3 v2) -> do codeGenLFunP lfp2 v2 a2
                                                   (id4, a4) <- getLastFunIdAndArity
                                                   codeGenLFunP lfp3 v3 a3
                                                   (id5, a5) <- getLastFunIdAndArity
                                                   genLineOfCode (APair a5 a4) ("pass_consts_para" <> id5 <> id4)

  (LMapP _, Atom 0, _) -> error "LMapP not meaningful for an Atom 0 argument"
  (LMapP lfp2, Atom n, v) -> do codeGenLFunP lfp2 v $ Atom $ n-1
                                (id2, _) <- getLastFunIdAndArity
                                genLineOfCode a1 ("constPassingMap" <> id2)

  (ZipP _, Atom 0, _) -> error "Zip not meaningful for an Atom 0 argument"
  (ZipP lfp2, Atom n, Tensor (hv:_)) -> do codeGenLFunP lfp2 hv $ Atom $ n-1
                                           (id2, _) <- getLastFunIdAndArity
                                           genLineOfCode a1 ("constPassingMap2" <> id2)
                                           -- TODO CALL verifyZipToMap2Legality
                                           -- TODO CALL verifyZipToMap2Legality
                                           -- TODO CALL verifyZipToMap2Legality
                                           -- TODO CALL verifyZipToMap2Legality

--- error section
  (RedP (List _), _, _)  -> error "Meaningless arity given to RedP."
  (ParaP _ _, _, _)      -> error "Meaningless arity given to ParaP."
  (FstP , _, _)          -> error "Meaningless arity given to FstP."
  (SndP , _, _)          -> error "Meaningless arity given to SndP."
  (AddP , _, _)          -> error "Meaningless arity given to AddP."
  (LMapP _, _, _)        -> error "Meaningless arity given to LMapP."
  (CompP _ _, _, _)      -> error "Wrong constants given to CompP"
  (ZipP _, _, _)         -> error "Wrong arguments given to ZipP"
-- missing impl
  (RedP _, _, _)         -> error "This relation not implemented in CodeGen"



inputArgDeclaration :: Arity -> Int -> (String, String, Int)
inputArgDeclaration a1 c1
  = case a1 of
  Atom _ -> ("(i" <> show c1 <> ": " <> typeDeclared a1 <> ")", "i" <> show c1, c1+1)
  APair a2 a3 -> let (params2, args2, c2) = inputArgDeclaration a2 c1 in
                 let (params3, args3, c3) = inputArgDeclaration a3 c2 in
                 (params2 <> " " <> params3, "(" <> args2 <> ", " <> args3 <> ")", c3)

typeDeclared :: Arity -> String
typeDeclared a = case a of
  Atom 0 -> "f32"
  Atom n -> "[]" <> typeDeclared (Atom $ n-1)
  APair a1 a2 -> "(" <> typeDeclared a1 <> ", " <> typeDeclared a2 <> ")"

put :: CState -> CodeGen ()
put cs = Co (\_ -> ((), cs))

getProgr :: CodeGen Program
getProgr = Co (\cs@(prog, _, _) -> (prog, cs))

finishProg :: Arity -> CodeGen ()
finishProg a =
  Co (\(p, r, c) ->
    let (params, args, _) = inputArgDeclaration a 0 in
    let new_loc = "entry main " <> params <>  " =" <> " fun" <> show (c-1) <> " " <> args <>"\n"
    in ((), (p <> new_loc, r, c)))

codeGenProgram :: LFun -> Arity -> Program
codeGenProgram lf arit =
  let initial = ("open import \"lmaplib\"\n\n", arit, 1) in
  let act = do put initial
               codeGenLFun lf arit
               finishProg arit
               getProgr
  in (fst . runCo act) initial

finishProgWithConsts :: Val -> Arity -> CodeGen ()
finishProgWithConsts consts a =
  Co (\(p, r, c) ->
    let (params, args, _) = inputArgDeclaration a 0 in
    let new_loc = "entry main " <> params <>  " =" <> " fun" <> show (c-1) <> " (" <> show consts <> ", " <> args <> ")"
    in ((), (p <> new_loc, r, c)))

codeGenProgramConstExtracted :: LFun -> Arity -> Program
codeGenProgramConstExtracted lf arit =
  let initial = ("open import \"lmaplib\"\n\n", arit, 1) in
  let (lf_extracted, consts) = extractLFunConsts lf in
  let act = do put initial
               codeGenLFunP lf_extracted consts arit
               finishProgWithConsts consts arit
               getProgr
  in (fst . runCo act) initial

--- please use this function to codegen
--- takes a sugary (if you want) lfun
--- and a val used as input to the lfun
 --- (necessary for arity-type disambiguation)
completeCodeGen :: LFun -> Val -> Program
completeCodeGen inlf vin =
          let lf = optimize (caramelizeLFun inlf) in
          let arit = getArity vin
          in  codeGenProgram lf arit
