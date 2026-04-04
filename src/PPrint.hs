module PPrint where

import AST
import Common
import Numeric (showFFloat)
import qualified Data.Vector as V
import Data.Text (unpack)
import Prettyprinter
import Prettyprinter.Render.Terminal( renderStrict,bold, color, colorDull, Color(..), AnsiStyle,  )
import Monad
import Global
import Control.Monad.Reader (asks)

data PPConf = PPConf { ppDecimals :: Int}

formatDouble :: PPConf -> Double -> String
formatDouble conf x = trimZeros (showFFloat (Just (ppDecimals conf)) x "")

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions


-- | Colors
numColor :: Doc AnsiStyle -> Doc AnsiStyle
numColor = annotate (color Red)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

varColor :: Doc AnsiStyle -> Doc AnsiStyle
varColor = annotate (color Yellow)

randomColor :: Doc AnsiStyle -> Doc AnsiStyle
randomColor = annotate (color Magenta)

functionColor :: Doc AnsiStyle -> Doc AnsiStyle
functionColor = annotate (color Blue <> bold)

nameColor :: Doc AnsiStyle -> Doc AnsiStyle
nameColor = annotate (color Cyan)


trimZeros :: String -> String
trimZeros s =
  case span (/= '.') s of
    (intPart, "") -> intPart
    (intPart, fracPart) ->
      let frac = drop 1 fracPart
          frac' = reverse (dropWhile (== '0') (reverse frac))
      in if null frac'
            then intPart
            else intPart ++ "." ++ frac'


-- | Numerics
int2Doc :: Int -> Doc AnsiStyle
int2Doc n = numColor (pretty n)

double2Doc :: PPConf -> Double -> Doc AnsiStyle
double2Doc conf n = numColor . pretty $ trimZeros (formatDouble conf n)

-- | Vectors
vec2Doc :: PPConf -> Vec Double -> Doc AnsiStyle
vec2Doc conf = tupled . map (double2Doc conf) . V.toList

vecInt2Doc :: Vec Int -> Doc AnsiStyle
vecInt2Doc = tupled . map int2Doc . V.toList


-- | Paths
path2Doc :: Path -> Doc AnsiStyle
path2Doc = brackets . hsep . punctuate comma . map (nameColor . pretty) . V.toList


-- | Operators
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


-- | Random Variables
varRand2Doc ::PPConf -> RandVar -> Doc AnsiStyle
varRand2Doc conf (Disc x) = varDisc2Doc conf x
varRand2Doc conf (Cont x) = varCont2Doc conf x


-- | Discrete Distributions
varDisc2Doc :: PPConf -> VarDisc -> Doc AnsiStyle
varDisc2Doc conf (Bin n p) =
  group (randomColor (pretty "Bin") <> parens (
  nest 2 (int2Doc n <> pretty "," <> line <> double2Doc conf p)))

varDisc2Doc conf (Poiss l) =
  group (randomColor (pretty "Poi") <> parens (
  nest 2 (double2Doc conf l)))

varDisc2Doc conf (Geo p) =
  group (randomColor (pretty "Geo") <> parens (
  nest 2 (double2Doc conf p)))

varDisc2Doc conf (Pasc r p) =
  group (randomColor (pretty "Pasc") <> parens (
  nest 2 (int2Doc r <> pretty "," <> line <> double2Doc conf p)))

varDisc2Doc conf (Hiper m r n) =
  group (randomColor (pretty "Hiper") <> parens (
  nest 2 (
    int2Doc m <> pretty "," <> line <>
    int2Doc r <> pretty "," <> line <>
    int2Doc n
  )))

varDisc2Doc conf (Custom v1 v2) =
  group (pretty "Custom" <> parens (
  nest 2 ( vecInt2Doc v1 <> pretty "," <> line <>
        vec2Doc conf v2)))


-- | Continous Distributions
varCont2Doc :: PPConf ->VarCont -> Doc AnsiStyle
varCont2Doc conf (Norm n m) =
  group (randomColor (pretty "Norm") <> parens (
  nest 2 (
    double2Doc conf n <> pretty "," <> line <>
    double2Doc conf m
  )))

varCont2Doc conf (Unif a b) =
  group (randomColor (pretty "Unif") <> parens (
  nest 2 (
    double2Doc conf a <> pretty "," <> line <>
    double2Doc conf b
  )))

varCont2Doc conf (Expo a) =
  randomColor (pretty "Expo") <> parens(
  nest 2 (double2Doc conf a))



-- | Random Expressions
expRand2Doc :: PPConf -> RandExp -> Doc AnsiStyle
expRand2Doc conf (DiscE x)  = expDisc2Doc conf x
expRand2Doc conf (ContE x)  = expCont2Doc conf x


-- | Discrete Expressions
expDisc2Doc :: PPConf -> ExpDisc -> Doc AnsiStyle
expDisc2Doc conf (BinE n p) =
  group (randomColor (pretty "Bin") <> parens (
  nest 2  (prettyExp conf n <> pretty "," <> line <> prettyExp conf p)))

expDisc2Doc conf (PoissE l) =
  group (randomColor (pretty "Poi") <> parens (
  nest 2 (prettyExp conf l)))

expDisc2Doc conf (GeoE p) =
  group (randomColor (pretty "Geo") <> parens (
  nest 2 (prettyExp conf p)))

expDisc2Doc conf (PascE r p) =
  group (randomColor (pretty "Pasc") <> parens (
  nest 2 (prettyExp conf r <> pretty ","  <> line <> prettyExp conf p)))

expDisc2Doc conf (HiperE m r n) = 
  group (randomColor (pretty "Hiper") <> parens(
  nest 2 (
    prettyExp conf m <> pretty "," <> line <>
    prettyExp conf r <> pretty "," <> line <>
    prettyExp conf n
  )))

expDisc2Doc conf (CustomE xs ps) = 
  group (brackets (prettyExp conf xs) <> pretty "," <> line 
    <> prettyExp conf ps)


-- Continous Expressions
expCont2Doc ::PPConf -> ExpCont -> Doc AnsiStyle
expCont2Doc conf (NormE n m) =
  group (randomColor (pretty "Norm") <> parens (
  nest 2 (
    prettyExp conf n <> pretty "," <> line <>
    prettyExp conf m
  )))

expCont2Doc conf (UnifE a b) =
  group (randomColor (pretty "Unif") <> parens (
  nest 2 (
    prettyExp conf a <> pretty "," <> line <>
    prettyExp conf b
  )))

expCont2Doc conf (ExpoE a) =
   group (randomColor (pretty "Expo") <> parens (
  nest 2 (prettyExp conf a)))




-- | Markov
markov2Doc :: PPConf -> Markov -> Doc AnsiStyle
markov2Doc conf (Mk names mat) = vsep  rows
          where
            ns = V.toList names
            ms = V.toList mat

            rows = zipWith rowDoc ns ms
            rowDoc name row = hsep [ nameColor (pretty name) , vec row]
            vec = brackets. hsep . punctuate comma . map (double2Doc conf). V.toList
            

-- | Main 
prettyExp :: PPConf -> Exp -> Doc AnsiStyle
prettyExp conf (ConstN i) = double2Doc conf i

prettyExp conf (VarRef x) = varColor (pretty x)

prettyExp conf (UMinus n) =
  pretty "-" <> maybeParenN conf n

prettyExp conf (OpNum Plus a b) =
  prettyExp conf a <+> opBin2Doc Plus <+> prettyExp conf b

prettyExp conf (OpNum Minus a b) =
  prettyExp conf a <+> opBin2Doc Minus <+> maybeParenN conf b

prettyExp conf (OpNum Times a b) =
  maybeParenN conf a <+> opBin2Doc Times <+> maybeParenN conf b

prettyExp conf (OpNum Div a b) =
  maybeParenN conf a <+> opBin2Doc Div <+> maybeParenN conf b

prettyExp conf (Access v n) =
  maybeParenN conf v <> brackets (prettyExp conf n)

prettyExp conf (Mean x) =
  functionColor (pretty "E") <> parens (prettyExp conf x)

prettyExp conf (Variance x) =
  functionColor (pretty "V") <> parens (prettyExp conf x)

prettyExp conf (StdDev x) =
  functionColor (pretty "D") <> parens (prettyExp conf x)

prettyExp conf (PDF x n) =
  group (
    functionColor (pretty "pdf") <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        prettyExp conf n
      )
    )
  )

prettyExp conf (MaxP x) =
  functionColor (pretty "maxP") <+> maybeParenN conf x

prettyExp conf (MaxPDF x) =
  functionColor (pretty "maxPDF") <+> prettyExp conf x

prettyExp conf (Prob x op n) =
  group (
    functionColor (pretty "P") <> parens (
      nest 2 (
        prettyExp conf x <+>
        opComp2Doc op <+>
        prettyExp conf n
      )
    )
  )

prettyExp conf (ProbBetween x op1 n op2 m) =
  group (
    functionColor (pretty "P") <> parens (
      nest 2 (
        prettyExp conf n <+>
        opComp2Doc op1 <+>
        prettyExp conf x <+>
        opComp2Doc op2 <+>
        prettyExp conf m
      )
    )
  )

prettyExp conf (ConstV v) =
  parens $ sep $ punctuate (pretty ",") (map (prettyExp conf) (V.toList v))

prettyExp conf (Mode x) =
  functionColor (pretty "mode") <+> prettyExp conf x

prettyExp conf (Rand x) =
  expRand2Doc conf x

prettyExp conf (ConstCh x) =
  path2Doc x

prettyExp conf (Markov (MarkovE x)) =
  functionColor (pretty "mk") <+> path2Doc x

prettyExp conf (ProbStep x i j n) =
  group (
    functionColor (pretty "F") <+> prettyExp conf n <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        nameColor (pretty i) <> pretty "," <> line <>
        nameColor (pretty j)
      )
    )
  )

prettyExp conf (ProbPath x c) =
  group (
    functionColor (pretty "F") <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        prettyExp conf c
      )
    )
  )

prettyExp conf (ProbHit c i j) =
  group (
    functionColor (pretty "F") <> parens (
      nest 2 (
        prettyExp conf c <> pretty "," <> line <>
        nameColor (pretty i) <> pretty "," <> line <>
        nameColor (pretty j)
      )
    )
  )

prettyExp conf (NextDist x n) =
  group (
    functionColor (pretty "ex") <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        prettyExp conf n
      )
    )
  )

prettyExp conf (Stationary x) =
  functionColor (pretty "stationary") <+> parens (prettyExp conf x)

prettyExp conf (SimulFromName x n i) =
  group (
    functionColor (pretty "simulate") <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        prettyExp conf i <> pretty "," <> line <>
        functionColor (pretty "start") <+> pretty "=" <+> varColor (pretty n)
      )
    )
  )

prettyExp conf (SimulFromVec x v i) =
  group (
    functionColor (pretty "simulate") <> parens (
      nest 2 (
        prettyExp conf x <> pretty "," <> line <>
        prettyExp conf i <> pretty "," <> line <>
        functionColor (pretty "prob") <+> pretty "=" <+> prettyExp conf v
      )
    )
  )


maybeParenN :: PPConf -> Exp -> Doc AnsiStyle
maybeParenN conf e@(OpNum Plus  _ _)  = parens (prettyExp conf e)
maybeParenN conf e@(OpNum Minus _ _)  = parens (prettyExp conf e)
maybeParenN conf e                    = prettyExp conf e


ppValue :: PPConf -> Value -> Doc AnsiStyle
ppValue conf (VRand x) = varRand2Doc conf x <> pretty "\n"
ppValue conf (VNum x)  = double2Doc conf x <> pretty "\n"
ppValue conf (VVec x)  = vec2Doc conf x <> pretty "\n"
ppValue conf (VMark x) = markov2Doc conf x <> pretty "\n"
ppValue conf (VPath x) = path2Doc x <> pretty "\n"
