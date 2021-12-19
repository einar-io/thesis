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

put :: CState -> CodeGen ()
put cs = Co (\_ -> ((), cs))

getProg :: CodeGen Program
getProg = Co (\cs@(prog, _, _) -> (prog, cs))

getLastFunIdAndArity :: CodeGen (String, Arity)
getLastFunIdAndArity = Co (\cs@(_, arit, cnt) -> ((" fun" <> show cnt, arit), cs))

completeCodeGen :: LFun -> Val -> Program
completeCodeGen lfun val = codeGenProgram (optimize (caramelizeLFun lfun)) (getArity val)

codeGenProgram :: LFun -> Arity -> Program
codeGenProgram lf arit =
  let initial = ("open import \"lmaplib\"\n\n", arit, 0)
      act = do codeGenLFun lf arit
               finishProg arit
               getProg
  in (fst . runCo act) initial

genLineOfCode :: Arity -> Program -> CodeGen ()
genLineOfCode outputArity fun =
  Co (\(program, _, counter) ->
      let newLineOfCode = "let" <> " fun" <> show (counter+1) <> " = (" <> fun <> ")" <> "\n"
      in ((), (program <> newLineOfCode, outputArity, counter+1)))

bilOp :: BilOp -> Arity -> Arity -> String
bilOp LossFunction a1 a2 = "lossFunction" <> arityAnnotation a1 a2
bilOp Outer      a1 a2 = "outer" <> arityAnnotation a1 a2
bilOp DotProd    a1 a2 = "inner" <> arityAnnotation a1 a2
bilOp MatrixMult a1 a2 = "inner" <> arityAnnotation a1 a2
bilOp VecMatProd a1 a2 = "inner" <> arityAnnotation a1 a2
bilOp MatVecProd a1 a2 = "inner" <> arityAnnotation a1 a2

arityAnnotation :: Arity -> Arity -> String
arityAnnotation a1 a2 = "_" <> show a1 <> "_" <> show a2

getReduceResultDim :: [(Int, Int)] -> Int
getReduceResultDim ls = let (_, dst) = unzip ls in 1 + maximum dst

codeGenLFun :: LFun -> Arity -> CodeGen ()
codeGenLFun Id a = genLineOfCode a "id"
codeGenLFun Dup a = genLineOfCode (APair a a) "dupe"
codeGenLFun Fst (APair a3 _) = genLineOfCode a3 "fst"
codeGenLFun Snd (APair _ a2) = genLineOfCode a2 "snd"
codeGenLFun Add (APair a3 a2) = genLineOfCode a2 ("add_" <> show a3 <> "_" <> show a2)
codeGenLFun Neg a = genLineOfCode a ("neg_" <> show a)
codeGenLFun (Red (List _)) (Atom 0) = error "Red not meaningful for an Atom 0 argument"
codeGenLFun (Red (List ls)) a@(Atom n) = genLineOfCode a ("reduce_" <> show n <> " " <> show ls <> " " <> show (getReduceResultDim ls))
codeGenLFun (LSec v op) a = genLineOfCode (bilOpOutputArity op a (getArity v)) (bilOp op (getArity v) a <> " " <> show v)
codeGenLFun (RSec op v) a = genLineOfCode (bilOpOutputArity op (getArity v) a) ("flip " <> bilOp op a (getArity v) <> " " <> show v)
codeGenLFun (Comp lf3 lf2) a1 =
    do codeGenLFun lf2 a1
       (id2, a2) <- getLastFunIdAndArity
       codeGenLFun lf3 a2
       (id3, a3) <- getLastFunIdAndArity
       genLineOfCode a3 ("comp" <> id3 <> id2)
codeGenLFun (Para lf3 lf2) (APair a3 a2) =
    do codeGenLFun lf2 a2
       (id4, a4) <- getLastFunIdAndArity
       codeGenLFun lf3 a3
       (id5, a5) <- getLastFunIdAndArity
       genLineOfCode (APair a5 a4) ("para" <> id5 <> id4)
codeGenLFun (LMap  _) (Atom 0) = error "LMap not meaningful for an Atom 0 argument"
codeGenLFun (LMap lf) a@(Atom n) =
    do codeGenLFun lf $ Atom $ n-1
       (id2, _) <- getLastFunIdAndArity
       genLineOfCode a ("map" <> id2)
codeGenLFun (Zip   _) (Atom 0) = error "zip not meaningful for an Atom 0 argument"
codeGenLFun (Zip lfs) a@(Atom n) =
    do let ((hf:_), vs@(hv:_)) = unzip $ map extractLFunConsts lfs
       codeGenLFunP hf hv (Atom $ n-1)
       (id2, _) <- getLastFunIdAndArity
       genLineOfCode a ("specMap2" <> id2 <> " " <> show vs)
codeGenLFun (Zip _) _         = error "illegal zip"
              --- error section
codeGenLFun (Red (List _)) _  = error "Meaningless arity given to Red."
codeGenLFun (Para _ _) _      = error "Meaningless arity given to Para."
codeGenLFun Fst _             = error "Meaningless arity given to Fst."
codeGenLFun Snd _             = error "Meaningless arity given to Snd."
codeGenLFun Add _             = error "Meaningless arity given to Add."
codeGenLFun (LMap _) _        = error "Meaningless arity given to LMap."
                -- desugared
codeGenLFun KZero _           = error "KZero should have been desugared!"
codeGenLFun (Scale _) _       = error "Scale should have been desugared!"
codeGenLFun (Prj _ _) _       = error "Prj should have been desugared!"
codeGenLFun (LPlus _ _) _     = error "LPlus should have been desugared!"
              -- missing impl
codeGenLFun (Red _) _         = error "This relation not implemented in CodeGen"

bilOpOutputArity :: BilOp -> Arity -> Arity -> Arity
bilOpOutputArity LossFunction (Atom 1) (Atom 1) = Atom 0
bilOpOutputArity Outer        (Atom n2) (Atom n1) = Atom $ n2+n1
bilOpOutputArity DotProd      (Atom n2) (Atom n1) = Atom $ n2+n1-2
bilOpOutputArity MatrixMult   (Atom n2) (Atom n1) = Atom $ n2+n1-2
bilOpOutputArity VecMatProd   (Atom n2) (Atom n1) = Atom $ n2+n1-2
bilOpOutputArity MatVecProd   (Atom n2) (Atom n1) = Atom $ n2+n1-2
bilOpOutputArity b a1 a2 = error $ "You called " <> show b <> " with illegal arities: " <> show a1 <> " and " <> show a2

genTypeDeclaration :: Arity -> String
genTypeDeclaration (Atom 0) = "f32"
genTypeDeclaration (Atom n) = "[]" <> genTypeDeclaration (Atom $ n-1)
genTypeDeclaration (APair a1 a2) = "(" <> genTypeDeclaration a1 <> ", " <> genTypeDeclaration a2 <> ")"

inputArgDeclaration :: Arity -> Int -> (String, String, Int)
inputArgDeclaration a1@(Atom _) c1 = ("(arg" <> show c1 <> ": " <> genTypeDeclaration a1 <> ")", "arg" <> show c1, c1+1)
inputArgDeclaration (APair a2 a3) c1 = let (params2, args2, c2) = inputArgDeclaration a2 c1
                                           (params3, args3, c3) = inputArgDeclaration a3 c2
                                        in (params2 <> " " <> params3, "(" <> args2 <> ", " <> args3 <> ")", c3)

finishProg :: Arity -> CodeGen ()
finishProg inputArity =
  Co (\(program, outputArity, counter) ->
    let (params, args, _) = inputArgDeclaration inputArity 1
        entryPoint = "entry main " <> params <>  " =" <> " fun" <> show counter <> " " <> args <> "\n"
    in ((), (program <> entryPoint, outputArity, counter)))

---------------------------------------- const extracted compiler below
---------------------------------------- const extracted compiler below

completeCodeGenConstExtracted :: LFun -> Val -> Program
completeCodeGenConstExtracted lfun val = codeGenProgramConstExtracted (optimize (caramelizeLFun lfun)) (getArity val)

codeGenProgramConstExtracted :: LFun -> Arity -> Program
codeGenProgramConstExtracted lf arit =
  let initial = ("open import \"lmaplib\"\n\n", arit, 0)
      (lf_extracted, consts) = extractLFunConsts lf
      act = do codeGenLFunP lf_extracted consts arit
               finishProgWithConsts consts arit
               getProg
  in (fst . runCo act) initial

finishProgWithConsts :: Val -> Arity -> CodeGen ()
finishProgWithConsts consts inputArity =
  Co (\(program, outputArity, counter) ->
    let (params, args, _) = inputArgDeclaration inputArity 1
        newLineOfCode = "entry main " <> params <>  " =" <> " fun" <> show counter <> " (" <> show consts <> ", " <> args <> ")"
    in ((), (program <> newLineOfCode, outputArity, counter)))

codeGenLFunP :: LFunP -> Val -> Arity -> CodeGen ()
codeGenLFunP IdP _ a = genLineOfCode a "ignoreDummyVal id"
codeGenLFunP DupP _ a = genLineOfCode (APair a a) "ignoreDummyVal dupe"
codeGenLFunP FstP _ (APair a3 _) = genLineOfCode a3 "ignoreDummyVal fst"
codeGenLFunP SndP _ (APair _ a2) = genLineOfCode a2 "ignoreDummyVal snd"
codeGenLFunP AddP _ (APair a3 a2) = genLineOfCode a2 ("ignoreDummyVal add_" <> show a3 <> "_" <> show a2)
codeGenLFunP NegP _  a = genLineOfCode a ("ignoreDummyVal neg_" <> show a)
codeGenLFunP (RedP (List _)) _ (Atom 0) = error "Red not meaningful for an Atom 0 argument"
codeGenLFunP (RedP (List ls)) _ a1@(Atom n) = genLineOfCode a1 ("ignoreDummyVal (reduce_" <> show n <> " " <> show ls <> " " <> show (getReduceResultDim ls) <> ")")

codeGenLFunP (LSecP op) v a = genLineOfCode (bilOpOutputArity op a (getArity v)) $ "uncurry " <> (bilOp op (getArity v) a)
codeGenLFunP (RSecP op) v a = genLineOfCode (bilOpOutputArity op (getArity v) a) $ "uncurry " <> ("(flip " <> bilOp op a (getArity v)) <> ")"

codeGenLFunP (CompP lfp3 lfp2) (Pair v3 v2) a1 =
      do codeGenLFunP lfp2 v2 a1
         (id2, a2) <- getLastFunIdAndArity
         codeGenLFunP lfp3 v3 a2
         (id3, a3) <- getLastFunIdAndArity
         genLineOfCode a3 ("constPassingComp" <> id3 <> id2)
codeGenLFunP (ParaP lfp3 lfp2) (Pair v3 v2) (APair a3 a2) =
      do codeGenLFunP lfp2 v2 a2
         (id4, a4) <- getLastFunIdAndArity
         codeGenLFunP lfp3 v3 a3
         (id5, a5) <- getLastFunIdAndArity
         genLineOfCode (APair a5 a4) ("constPassingPara" <> id5 <> id4)

codeGenLFunP (LMapP _) _ (Atom 0) = error "LMapP not meaningful for an Atom 0 argument"
codeGenLFunP (LMapP lfp2) v a1@(Atom n) =
      do codeGenLFunP lfp2 v $ Atom $ n-1
         (id2, _) <- getLastFunIdAndArity
         genLineOfCode a1 ("constPassingMap" <> id2)

codeGenLFunP (ZipP _) _ (Atom 0) = error "Zip not meaningful for an Atom 0 argument"
codeGenLFunP (ZipP lfp2) (Vector (hv:_)) a1@(Atom n) =
      do codeGenLFunP lfp2 hv $ Atom $ n-1
         (id2, _) <- getLastFunIdAndArity
         genLineOfCode a1 ("constPassingMap2" <> id2)

--- error section
codeGenLFunP (RedP (List _)) _ _  = error "Meaningless arity given to RedP."
codeGenLFunP (ParaP _ _) _ _      = error "Meaningless arity given to ParaP."
codeGenLFunP FstP _ _             = error "Meaningless arity given to FstP."
codeGenLFunP SndP _ _             = error "Meaningless arity given to SndP."
codeGenLFunP AddP _ _             = error "Meaningless arity given to AddP."
codeGenLFunP (LMapP _) _ _        = error "Meaningless arity given to LMapP."
codeGenLFunP (CompP _ _) _ _      = error "Wrong constants given to CompP"
codeGenLFunP (ZipP _) _ _         = error "Wrong arguments given to ZipP"
-- missing impl
codeGenLFunP (RedP _) _ _         = error "This relation not implemented in CodeGen"
