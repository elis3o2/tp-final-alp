module PPrint where

import AST
import Common

import Numeric (showFFloat)

import qualified Data.Vector as V
import Data.Text (unpack)
import Prettyprinter
  ( (<+>)
  , annotate
  , parens
  , brackets
  , pretty
  , Doc
  , nest
  , group
  , line
  , sep
  , punctuate
  , defaultLayoutOptions
  , layoutSmart
  )
import Prettyprinter.Render.Terminal( renderStrict,bold, color, colorDull, Color(..), AnsiStyle )

formatDouble :: Double -> String
formatDouble x = showFFloat (Just 24) x ""


render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- Colores
numColor :: Doc AnsiStyle -> Doc AnsiStyle
numColor = annotate (color Blue)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

varColor :: Doc AnsiStyle -> Doc AnsiStyle
varColor = annotate (color Yellow)

randomColor :: Doc AnsiStyle -> Doc AnsiStyle
randomColor = annotate (color Magenta)

functionColor :: Doc AnsiStyle -> Doc AnsiStyle
functionColor = annotate (color Red <> bold)


parenIf :: Bool -> Doc a -> Doc a
parenIf True  = parens
parenIf False = id

-- NumC
numC2Doc :: NumC -> Doc AnsiStyle
numC2Doc (I n) = numColor (pretty n)
numC2Doc (D n) = numColor (pretty n)

int2Doc :: Int -> Doc AnsiStyle
int2Doc n = numColor (pretty n)

double2Doc :: Double -> Doc AnsiStyle
double2Doc n = numColor (pretty n)

-- Operadores
opBin2Doc :: OpBin -> Doc AnsiStyle
opBin2Doc Plus  = opColor (pretty "+")
opBin2Doc Minus = opColor (pretty "-")
opBin2Doc Times = opColor (pretty "*")
opBin2Doc Div   = opColor (pretty "/")

opComp2Doc :: OpComp -> Doc AnsiStyle
opComp2Doc Lt  = opColor (pretty "<")
opComp2Doc Gt  = opColor (pretty ">")
opComp2Doc Lte = opColor (pretty "<=")
opComp2Doc Gte = opColor (pretty ">=")
opComp2Doc Eq  = opColor (pretty "=")
opComp2Doc NEq = opColor (pretty "/=")


-- Distribuciones discretas
varDisc2Doc :: VarDisc -> Doc AnsiStyle
varDisc2Doc (Bin n p) =
  group (randomColor (pretty "Bin") <> parens (
  nest 2 (int2Doc n <> pretty "," <> line <> double2Doc p)))

varDisc2Doc (Poiss l) =
  group (randomColor (pretty "Poi") <> parens (
  nest 2 (double2Doc l)))

varDisc2Doc (Geo p) =
  group (randomColor (pretty "Geo") <> parens (
  nest 2 (double2Doc p)))

varDisc2Doc (Pasc r p) =
  group (randomColor (pretty "Pasc") <> parens (
  nest 2 (int2Doc r <> pretty "," <> line <> double2Doc p)))

varDisc2Doc (Hiper m r n) =
  group (randomColor (pretty "Hiper") <> parens (
  nest 2 (
    int2Doc m <> pretty "," <> line <>
    int2Doc r <> pretty "," <> line <>
    int2Doc n
  )))

varDisc2Doc (Custom v1 v2) =
  pretty "FALTA"

-- Distribuciones continuas
varCont2Doc :: VarCont -> Doc AnsiStyle
varCont2Doc (Norm n m) =
  group (randomColor (pretty "Norm") <> parens (
  nest 2 (
    double2Doc n <> pretty "," <> line <>
    double2Doc m
  )))

varCont2Doc (Unif a b) =
  group (randomColor (pretty "Unif") <> parens (
  nest 2 (
    double2Doc a <> pretty "," <>
    double2Doc b
  )))

varCont2Doc (Expo a) =
  randomColor (pretty "Expo") <> parens(
  nest 2 (double2Doc a))

varAle2Doc :: RandVar -> Doc AnsiStyle
varAle2Doc (Disc x) = varDisc2Doc x
varAle2Doc (Cont x) = varCont2Doc x

-- Expresiones discretas
expDisc2Doc :: ExpDisc -> Doc AnsiStyle
expDisc2Doc (BinE n p) =
  group (randomColor (pretty "Bin") <> parens (
  nest 2  (prettyExp n <> pretty "," <> line <> prettyExp p)))

expDisc2Doc (PoissE l) =
  group (randomColor (pretty "Poi") <> parens (
  nest 2 (prettyExp l)))

expDisc2Doc (GeoE p) =
  group (randomColor (pretty "Geo") <> parens (
  nest 2 (prettyExp p)))

expDisc2Doc (PascE r p) =
  group (randomColor (pretty "Pasc") <> parens (
  nest 2 (prettyExp r <> pretty ","  <> prettyExp p)))

expDisc2Doc (HiperE m r n) = 
  group (randomColor (pretty "Hiper") <> parens(
  nest 2 (
    prettyExp m <> pretty "," <> line <>
    prettyExp r <> pretty "," <> line <>
    prettyExp n
  )))

expDisc2Doc _ =pretty "FALTA"

-- Expresiones continuas
expCont2Doc :: ExpCont -> Doc AnsiStyle
expCont2Doc (NormE n m) =
  group (randomColor (pretty "Norm") <> parens (
  nest 2 (
    prettyExp n <> pretty "," <> line <>
    prettyExp m
  )))

expCont2Doc (UnifE a b) =
  group (randomColor (pretty "Unif") <> parens (
  nest 2 (
    prettyExp a <> pretty "," <> line <>
    prettyExp b
  )))

expCont2Doc (ExpoE a) =
   group (randomColor (pretty "Expo") <> parens (
  nest 2 (prettyExp a)))

expAle2Doc :: RandExp -> Doc AnsiStyle
expAle2Doc (DiscE x)  = expDisc2Doc x
expAle2Doc (ContE x)  = expCont2Doc x






ppValue ::  Value -> String
ppValue (VAle x) = render (varAle2Doc x)
ppValue (VNum x) = render (numC2Doc x)
ppValue (VVec x) = render (parens $ sep $ punctuate (pretty ",") (map numC2Doc (V.toList x)))



prettyExp :: Exp -> Doc AnsiStyle
prettyExp (ConstN i) = numC2Doc i
prettyExp (VarRef x)    = varColor (pretty x)
prettyExp (UMinus n) = pretty "-" <> maybeParenN n
prettyExp (OpNum Plus  a b) = prettyExp a <+> opBin2Doc Plus <+> prettyExp b
prettyExp (OpNum Minus a b) = prettyExp a <+> opBin2Doc Minus <+> maybeParenN b
prettyExp (OpNum Times a b) = maybeParenN a <+> opBin2Doc Times <+> maybeParenN b
prettyExp (OpNum Div   a b) = maybeParenN a <+> opBin2Doc Div   <+> maybeParenN b
prettyExp (Access v n) = maybeParenN v <> brackets (prettyExp n)
prettyExp (Mean x)  = functionColor (pretty "E") <> parens (prettyExp x)
prettyExp (Variance x) = functionColor (pretty "V") <> parens (prettyExp x)
prettyExp (StdDev x) = functionColor (pretty "D") <> parens (prettyExp x)
prettyExp (FDP x n) = functionColor (pretty "fdp") <> parens (prettyExp x <+> prettyExp n)
prettyExp (MaxP x) = functionColor (pretty "maxP") <+> prettyExp x
prettyExp (MaxFDP x)= functionColor (pretty "maxFDP") <> prettyExp x
prettyExp (Prob x op n)          = functionColor (pretty "P") <> parens (
                                                          prettyExp x <+>
                                                          opComp2Doc op
                                                          <+> prettyExp n)
prettyExp (ProbBetween x op1 n op2 m) = functionColor (pretty "P") <> parens (
                                                            prettyExp n <+>
                                                            opComp2Doc op1  <+>
                                                            prettyExp x  <+>
                                                            opComp2Doc op2 <+>
                                                            prettyExp m
                                                          )
prettyExp (ConstV v) = parens $ sep $ punctuate (pretty ",") (map prettyExp (V.toList v))
prettyExp (Mode x) = functionColor (pretty "moda") <+> prettyExp x
prettyExp (Rand x) = expAle2Doc x






maybeParenN :: Exp -> Doc AnsiStyle
maybeParenN e@(OpNum Plus  _ _)  = parens (prettyExp e)
maybeParenN e@(OpNum Minus _ _)  = parens (prettyExp e)
maybeParenN e                    = prettyExp e


