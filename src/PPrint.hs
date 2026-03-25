module PPrint where

import AST
import Common
import Monad
import Eval
import Global
import Data.List (transpose)
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
  , sep
  , punctuate
  , defaultLayoutOptions
  , layoutSmart
  )
import Prettyprinter.Render.Terminal( renderStrict, color, colorDull, Color(..), AnsiStyle )
import qualified Text.PrettyPrint.Boxes as B

formatDouble :: Double -> String
formatDouble x = showFFloat (Just 24) x ""


render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- Colores
constColor :: Doc AnsiStyle -> Doc AnsiStyle
constColor = annotate (color Red)

opColor :: Doc AnsiStyle -> Doc AnsiStyle
opColor = annotate (colorDull Green)

keywordColor :: Doc AnsiStyle -> Doc AnsiStyle
keywordColor = annotate (colorDull Green)

parenIf :: Bool -> Doc a -> Doc a
parenIf True  = parens
parenIf False = id

-- NumC
numC2Doc :: NumC -> Doc AnsiStyle
numC2Doc (I n) = constColor (pretty n)
numC2Doc (D n) = constColor (pretty n)

int2Doc :: Int -> Doc AnsiStyle
int2Doc n = constColor (pretty n)

double2Doc :: Double -> Doc AnsiStyle
double2Doc n = constColor (pretty n)

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
  keywordColor (pretty "Bin") <>
  parens (int2Doc n <> pretty "," <+> double2Doc p)

varDisc2Doc (Poiss l) =
  keywordColor (pretty "Poi") <>
  parens (double2Doc l)

varDisc2Doc (Geo p) =
  keywordColor (pretty "Geo") <>
  parens (double2Doc p)

varDisc2Doc (Pasc r p) =
  keywordColor (pretty "Pasc") <>
  parens (int2Doc r <> pretty "," <+> double2Doc p)

varDisc2Doc (Hiper m r n) =
  keywordColor (pretty "Hiper") <>
  parens (
    int2Doc m <> pretty "," <+>
    int2Doc r <> pretty "," <+>
    int2Doc n
  )

-- Distribuciones continuas
varCont2Doc :: VarCont -> Doc AnsiStyle
varCont2Doc (Norm n m) =
  keywordColor (pretty "Norm") <>
  parens (
    double2Doc n <> pretty "," <+>
    double2Doc m
  )

varCont2Doc (Unif a b) =
  keywordColor (pretty "Unif") <>
  parens (
    double2Doc a <> pretty "," <+>
    double2Doc b
  )

varCont2Doc (Expo a) =
  keywordColor (pretty "Expo") <>
  parens (double2Doc a)

varAle2Doc :: VarAle -> Doc AnsiStyle
varAle2Doc (Disc x) = varDisc2Doc x
varAle2Doc (Cont x) = varCont2Doc x

-- Expresiones discretas
expDisc2Doc :: ExpDisc -> Doc AnsiStyle
expDisc2Doc (BinE n p) =
  keywordColor (pretty "Bin") <>
  parens (expNum2Doc n <> pretty "," <+> expNum2Doc p)

expDisc2Doc (PoissE l) =
  keywordColor (pretty "Poi") <>
  parens (expNum2Doc l)

expDisc2Doc (GeoE p) =
  keywordColor (pretty "Geo") <>
  parens (expNum2Doc p)

expDisc2Doc (PascE r p) =
  keywordColor (pretty "Pasc") <>
  parens (expNum2Doc r <> pretty "," <+> expNum2Doc p)

expDisc2Doc (HiperE m r n) =
  keywordColor (pretty "Hiper") <>
  parens (
    expNum2Doc m <> pretty "," <+>
    expNum2Doc r <> pretty "," <+>
    expNum2Doc n
  )

-- Expresiones continuas
expCont2Doc :: ExpCont -> Doc AnsiStyle
expCont2Doc (NormE n m) =
  keywordColor (pretty "Norm") <>
  parens (
    expNum2Doc n <> pretty "," <+>
    expNum2Doc m
  )

expCont2Doc (UnifE a b) =
  keywordColor (pretty "Unif") <>
  parens (
    expNum2Doc a <> pretty "," <+>
    expNum2Doc b
  )

expCont2Doc (ExpoE a) =
  keywordColor (pretty "Expo") <>
  parens (expNum2Doc a)

expAle2Doc :: ExpAle -> Doc AnsiStyle
expAle2Doc (VarA x)   = pretty (show x)
expAle2Doc (DiscE x)  = expDisc2Doc x
expAle2Doc (ContE x)  = expCont2Doc x

-- Expresiones numéricas
expNum2Doc :: ExpNum -> Doc AnsiStyle
expNum2Doc (ConstN i) = numC2Doc i
expNum2Doc (VarN x)   = pretty (show x)

expNum2Doc (UMinus n) =
  pretty "-" <> expNum2Doc n

expNum2Doc (OpNum Plus a b) =
  expNum2Doc a <+> opBin2Doc Plus <+> expNum2Doc b

expNum2Doc (OpNum Minus a b) =
  expNum2Doc a <+> opBin2Doc Minus <+> maybeParenN b

expNum2Doc (OpNum Times a b) =
  maybeParenN a <+> opBin2Doc Times <+> maybeParenN b

expNum2Doc (OpNum Div a b) =
  maybeParenN a <+> opBin2Doc Div <+> maybeParenN b

expNum2Doc (Access v n) =
  maybeParenV v <> brackets (expNum2Doc n)

expNum2Doc (Esp x) =
  keywordColor (pretty "E") <> parens (expAle2Doc x)

expNum2Doc (Vari x) =
  keywordColor (pretty "V") <> parens (expAle2Doc x)

expNum2Doc (Desv x) =
  keywordColor (pretty "D") <> parens (expAle2Doc x)

expNum2Doc (FDP x n) =
  keywordColor (pretty "fdp") <> parens (expAle2Doc x <+> expNum2Doc n)

expNum2Doc (MaxP x) =
  keywordColor (pretty "maxP") <+> expAle2Doc x

-- Ajusta este constructor según tu AST:
-- si es MaxFDP ExpAle, usa el argumento; si no, quita "x".
expNum2Doc (MaxFDP x)=
  keywordColor (pretty "maxFDP") <> expAle2Doc x 

expNum2Doc (POp x op n) =
  keywordColor (pretty "P") <>
  parens (expAle2Doc x <+> opComp2Doc op <+> expNum2Doc n)

expNum2Doc (POpBt x op1 n op2 m) =
  keywordColor (pretty "P") <>
  parens (
    expNum2Doc n <+> opComp2Doc op1  <+>
    expAle2Doc x  <+>
    opComp2Doc op2 <+> expNum2Doc m
  )

maybeParenN :: ExpNum -> Doc AnsiStyle
maybeParenN e@(OpNum Plus  _ _)  = parens (expNum2Doc e)
maybeParenN e@(OpNum Minus _ _)  = parens (expNum2Doc e)
maybeParenN e                    = expNum2Doc e



maybeParenV :: ExpVec -> Doc AnsiStyle
maybeParenV e@(Moda _) = parens (expVec2Doc e)
maybeParenV e          = expVec2Doc e

-- Vec
vec2Doc :: Vec NumC -> Doc AnsiStyle
vec2Doc v = parens $
    sep $
      punctuate (pretty ",") (map numC2Doc (V.toList v))



expVec2Doc :: ExpVec -> Doc AnsiStyle
expVec2Doc (VarV v) =
  pretty v

expVec2Doc (ConstV v) =
  parens $
    sep $
      punctuate (pretty ",") (map expNum2Doc (V.toList v))

expVec2Doc (Moda x) =
  keywordColor (pretty "moda") <+> expAle2Doc x


ppValue ::  Value -> String 
ppValue (VAle x) = render (varAle2Doc x)
ppValue (VNum x) = render (numC2Doc x)
ppValue (VVec x) = render (vec2Doc x)



renderTable :: [String] -> [[String]] -> String
renderTable headers rows =
  B.render $
    B.vsep 0 B.left $
      [ sepLine
      , rowHeader headers
      , sepLine
      ]
      ++ concatMap (\r -> [row r, sepLine]) rows
  where
    -- ancho de cada columna
    colWidths =
      map (maximum . map length) $
        transpose (headers : rows)

    -- alinear derecha (para números)
    padLeft w s = replicate (w - length s) ' ' ++ s

    -- centrar (para headers)
    padCenter w s =
      let total = w - length s
          left  = total `div` 2
          right = total - left
      in replicate left ' ' ++ s ++ replicate right ' '

    -- fila normal
    row xs =
      B.hcat B.left $
        [B.text "|"]
        ++ concat
            [ [ B.text (" " ++ padLeft w x ++ " ")
              , B.text "|"
              ]
            | (w, x) <- zip colWidths xs
            ]

    -- header centrado
    rowHeader xs =
      B.hcat B.left$
        [B.text "|"]
        ++ concat
            [ [ B.text (" " ++ padCenter w x ++ " ")
              , B.text "|"
              ]
            | (w, x) <- zip colWidths xs
            ]

    -- línea separadora
    sepLine =
      B.text $
        concatMap (\w -> "+" ++ replicate (w + 2) '-') colWidths ++ "+"


getValuesDisc :: VarDisc -> [[String]]
getValuesDisc x@(Bin n p) = [[show k, formatDouble(getProbDisc x Eq k)] | k <- [0..n] ]
getValuesDisc _ = [[]] 


discTable :: VarAle -> String
discTable (Disc x) = renderTable ["X", "P(X)"] (getValuesDisc x)
discTable _ = ""