{-|
Module      : PPrint
Description : Pretty printer for expressions, values and commands.
-}

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

formatDouble :: MonadProb m => Double -> m String
formatDouble x = do d <- asks decimals
                    return $ trimZeros (showFFloat (Just d) x "")

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
nameColor = annotate (color Green)


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

double2Doc :: MonadProb m => Double -> m (Doc AnsiStyle)
double2Doc n = do s <- formatDouble n
                  return $ numColor (pretty s)

-- | Vectors
vec2Doc :: MonadProb m => Vec Double -> m (Doc AnsiStyle)
vec2Doc v = do ds <- mapM double2Doc (V.toList v)
               return $ tupled ds

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


flipOp :: OpComp -> OpComp
flipOp Lt  = Gt
flipOp Gt  = Lt
flipOp Lte = Gte
flipOp Gte = Lte
flipOp op  = op


-- | Random Variables
varRand2Doc :: MonadProb m => RandVar -> m (Doc AnsiStyle)
varRand2Doc (Disc x) = varDisc2Doc x
varRand2Doc (Cont x) = varCont2Doc x


-- | Discrete Distributions
varDisc2Doc :: MonadProb m => VarDisc -> m (Doc AnsiStyle)
varDisc2Doc (Bin n p) = do pd <- double2Doc p
                           return $
                             group (randomColor (pretty "Bin") <> parens (
                             nest 2 (int2Doc n <> pretty "," <> line <> pd)))

varDisc2Doc (Poiss l) = do pl <- double2Doc l
                           return $
                             group (randomColor (pretty "Poi") <> parens (
                             nest 2 pl))

varDisc2Doc (Geo p) = do pp <- double2Doc p
                         return $
                           group (randomColor (pretty "Geo") <> parens (
                           nest 2 pp))

varDisc2Doc (Pasc r p) = do pp <- double2Doc p
                            return $
                              group (randomColor (pretty "BN") <> parens (
                              nest 2 (int2Doc r <> pretty "," <> line <> pp)))

varDisc2Doc (Hiper m r n) = do return $
                                group (randomColor (pretty "HG") <> parens (
                                nest 2 (
                                  int2Doc m <> pretty "," <> line <>
                                  int2Doc r <> pretty "," <> line <>
                                  int2Doc n
                                )))

varDisc2Doc (Custom v1 v2) = do pv2 <- vec2Doc v2
                                return $
                                  group (brackets (
                                    nest 2 (vecInt2Doc v1 <> line <> pv2)))

-- | Continous Distributions
varCont2Doc :: MonadProb m => VarCont -> m (Doc AnsiStyle)
varCont2Doc (Norm n m) = do pn <- double2Doc n
                            pm <- double2Doc m
                            return $
                              group (randomColor (pretty "N") <> parens (
                              nest 2 (
                                pn <> pretty "," <> line <>
                                pm
                              )))

varCont2Doc (Unif a b) = do pa <- double2Doc a
                            pb <- double2Doc b
                            return $
                              group (randomColor (pretty "Unif") <> parens (
                              nest 2 (
                                pa <> pretty "," <> line <>
                                pb
                              )))

varCont2Doc (Expo a) = do pa <- double2Doc a
                          return $
                            randomColor (pretty "Exp") <> parens(
                            nest 2 pa)



-- | Random Expressions
expRand2Doc :: MonadProb m => RandExp -> m (Doc AnsiStyle)
expRand2Doc (DiscE x)  = expDisc2Doc x
expRand2Doc (ContE x)  = expCont2Doc x


-- | Discrete Expressions
expDisc2Doc :: MonadProb m => ExpDisc -> m (Doc AnsiStyle)
expDisc2Doc (BinE n p) = do pn <- prettyExp n
                            pp <- prettyExp p
                            return $
                              group (randomColor (pretty "Bin") <> parens (
                              nest 2  (pn <> pretty "," <> line <> pp)))

expDisc2Doc (PoissE l) = do pl <- prettyExp l
                            return $
                              group (randomColor (pretty "Poi") <> parens (
                              nest 2 pl))

expDisc2Doc (GeoE p) = do pp <- prettyExp p
                          return $
                            group (randomColor (pretty "Geo") <> parens (
                            nest 2 pp))

expDisc2Doc (PascE r p) = do pr <- prettyExp r
                             pp <- prettyExp p
                             return $
                               group (randomColor (pretty "BN") <> parens (
                               nest 2 (pr <> pretty ","  <> line <> pp)))

expDisc2Doc (HiperE m r n) = do pm <- prettyExp m
                                pr <- prettyExp r
                                pn <- prettyExp n
                                return $
                                  group (randomColor (pretty "HG") <> parens(
                                  nest 2 (
                                    pm <> pretty "," <> line <>
                                    pr <> pretty "," <> line <>
                                    pn
                                  )))

expDisc2Doc (CustomE xs ps) = do pxs <- prettyExp xs
                                 pps <- prettyExp ps
                                 return $
                                   group (brackets (pxs <> line <> pps))


-- Continous Expressions
expCont2Doc :: MonadProb m => ExpCont -> m (Doc AnsiStyle)
expCont2Doc (NormE n m) = do pn <- prettyExp n
                             pm <- prettyExp m
                             return $
                               group (randomColor (pretty "N") <> parens (
                               nest 2 (
                                 pn <> pretty "," <> line <>
                                 pm
                               )))

expCont2Doc (UnifE a b) = do pa <- prettyExp a
                             pb <- prettyExp b
                             return $
                               group (randomColor (pretty "Unif") <> parens (
                               nest 2 (
                                 pa <> pretty "," <> line <>
                                 pb
                               )))

expCont2Doc (ExpoE a) = do pa <- prettyExp a
                           return $
                             group (randomColor (pretty "Exp") <> parens (
                             nest 2 pa))


-- | Markov
markov2Doc :: MonadProb m => Markov -> m (Doc AnsiStyle)
markov2Doc (Mk names mat) = do
          ds <- mapM vec2Doc (V.toList mat)
          let ns   = V.toList names
              rows = zipWith rowDoc ns ds
          return $ vsep rows
          where
            rowDoc name row = hsep [ nameColor (pretty name) , row]
            

-- | Main 
prettyExp :: MonadProb m => Exp -> m (Doc AnsiStyle)
prettyExp (ConstN i) = double2Doc i

prettyExp (VarRef x) = return $ varColor (pretty x)

prettyExp (UMinus n) = do
  pn <- maybeParenN n
  return $ pretty "-" <> pn

prettyExp (OpNum Plus a b) = do
  pa <- prettyExp a
  pb <- prettyExp b
  return $ pa <+> opBin2Doc Plus <+> pb

prettyExp (OpNum Minus a b) = do
  pa <- prettyExp a
  pb <- maybeParenN b
  return $ pa <+> opBin2Doc Minus <+> pb

prettyExp (OpNum Times a b) = do
  pa <- maybeParenN a
  pb <- maybeParenN b
  return $ pa <+> opBin2Doc Times <+> pb

prettyExp (OpNum Div a b) = do
  pa <- maybeParenN a
  pb <- maybeParenN b
  return $ pa <+> opBin2Doc Div <+> pb

prettyExp (Access v n) = do
  pv <- maybeParenN v
  pn <- prettyExp n
  return $ pv <> brackets pn

prettyExp (Mean x) = do
  px <- prettyExp x
  return $ functionColor (pretty "E") <> parens px

prettyExp (Variance x) = do
  px <- prettyExp x
  return $ functionColor (pretty "V") <> parens px

prettyExp (StdDev x) = do
  px <- prettyExp x
  return $ functionColor (pretty "SD") <> parens px

prettyExp (PDF x n) = do
  px <- prettyExp x
  pn <- prettyExp n
  return $
    group (
      functionColor (pretty "pdf") <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          pn
        )
      )
    )

prettyExp (MaxP x) = do
  px <- maybeParenN x
  return $ functionColor (pretty "maxP") <+> px

prettyExp (MaxPDF x) = do
  px <- prettyExp x
  return $ functionColor (pretty "maxPDF") <+> px

prettyExp (Prob x op n) = do
  px <- prettyExp x
  pn <- prettyExp n
  return $
    group (
      functionColor (pretty "P") <> parens (
        nest 2 (
          px <+>
          opComp2Doc op <+>
          pn
        )
      )
    )

prettyExp (ProbBetween x op1 n op2 m) = do
  px <- prettyExp x
  pn <- prettyExp n
  pm <- prettyExp m
  return $
    group (
      functionColor (pretty "P") <> parens (
        nest 2 (
          pn <+>
          opComp2Doc (flipOp op1) <+>
          px <+>
          opComp2Doc op2 <+>
          pm
        )
      )
    )

prettyExp (ConstV v) = do
  ps <- mapM prettyExp (V.toList v)
  return $ parens $ sep $ punctuate (pretty ",") ps

prettyExp (Mode x) = do
  px <- prettyExp x
  return $ functionColor (pretty "mode") <+> px

prettyExp (Rand x) = expRand2Doc x

prettyExp (ConstP x) = return $ path2Doc x

prettyExp (Markov (MarkovE x)) =
  return $ functionColor (pretty "mk") <+> path2Doc x

prettyExp (ProbStep x i j n) = do
  px <- prettyExp x
  pn <- prettyExp n
  return $
    group (
      functionColor (pretty "F") <+> pn <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          nameColor (pretty i) <> pretty "," <> line <>
          nameColor (pretty j)
        )
      )
    )

prettyExp (ProbPath x c) = do
  px <- prettyExp x
  pc <- prettyExp c
  return $
    group (
      functionColor (pretty "F") <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          pc
        )
      )
    )

prettyExp (ProbHit c i j) = do
  pc <- prettyExp c
  return $
    group (
      functionColor (pretty "F") <> parens (
        nest 2 (
          pc <> pretty "," <> line <>
          nameColor (pretty i) <> pretty "," <> line <>
          nameColor (pretty j)
        )
      )
    )

prettyExp (NextDist x n) = do
  px <- prettyExp x
  pn <- prettyExp n
  return $
    group (
      functionColor (pretty "ex") <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          pn
        )
      )
    )

prettyExp (Stationary x) = do
  px <- prettyExp x
  return $ functionColor (pretty "stationary") <+> parens px

prettyExp (SimulFromName x n i) = do
  px <- prettyExp x
  ppi <- prettyExp i
  return $
    group (
      functionColor (pretty "simulate") <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          ppi <> pretty "," <> line <>
          functionColor (pretty "start") <+> pretty "=" <+> varColor (pretty n)
        )
      )
    )

prettyExp (SimulFromVec x v i) = do
  px <- prettyExp x
  pv <- prettyExp v
  ppi <- prettyExp i
  return $
    group (
      functionColor (pretty "simulate") <> parens (
        nest 2 (
          px <> pretty "," <> line <>
          ppi <> pretty "," <> line <>
          functionColor (pretty "prob") <+> pretty "=" <+> pv
        )
      )
    )


maybeParenN :: MonadProb m => Exp -> m (Doc AnsiStyle)
maybeParenN e@(OpNum Plus  _ _) = parens <$> prettyExp e
maybeParenN e@(OpNum Minus _ _) = parens <$> prettyExp e
maybeParenN e                   = prettyExp e



ppNodesExp :: MonadProb m => NodeExp -> m (Doc AnsiStyle)
ppNodesExp (NE node) = do
  ps <- mapM (\(n, v) -> do pv <- prettyExp v
                            return $ parens (nameColor (pretty n) <> pretty "," <> pv)) node
  return $ brackets . hsep . punctuate comma $ ps


ppNode :: MonadProb m => NodeVal -> m (Doc AnsiStyle)
ppNode (N node) = do
  ds <- mapM (\(n, p) -> do pp <- double2Doc p
                            return (nameColor (pretty n) <+> pretty ":" <+> pp)) node
  return $ brackets (hsep (punctuate (pretty ",") ds)) <> pretty "\n"


ppComm :: MonadProb m => Comm -> m (Doc AnsiStyle)
ppComm (Let x e) = do ty <- getTy x
                      pe <- prettyExp e
                      let op = case ty of
                                 RandCont -> pretty "~"
                                 RandDisc -> pretty "~"
                                 _        -> pretty "="
                      return $ group (varColor (pretty x) <+> op <+> nest 2 pe)

ppComm (Print x) = do px <- prettyExp x
                      return $ group (functionColor (pretty "print") <> parens px)

ppComm (Table x) = do px <- prettyExp x
                      return $ group (functionColor (pretty "table") <> parens px)

ppComm (TableR x n m) = do px <- prettyExp x
                           pn <- prettyExp n
                           pm <- prettyExp m
                           return $ group (
                              functionColor (pretty "table") <> parens (
                                nest 2 (
                                  px <> pretty "," <> line <>
                                  pn <> pretty "," <> line <>
                                  pm)))

ppComm (Plot x) = do px <- prettyExp x
                     return $ group (functionColor (pretty "plot") <> parens px)

ppComm (LetN x n) = do pe <- ppNodesExp n
                       return $ group (
                         nameColor (pretty x) <> pretty " ->" <>
                         nest 2 (line <> pe))


ppValue :: MonadProb m => Value -> m (Doc AnsiStyle)
ppValue (VRand x) = do pd <- varRand2Doc x
                       return $ pd <> pretty "\n"
ppValue (VNum x)  = do pd <- double2Doc x
                       return $ pd <> pretty "\n"
ppValue (VVec x)  = do pd <- vec2Doc x
                       return $ pd <> pretty "\n"
ppValue (VMark x) = do pd <- markov2Doc x
                       return $ pd <> pretty "\n"
ppValue (VPath x) = return $ path2Doc x <> pretty "\n"
ppValue (VNode x) = do pd <- ppNode x  
                       return $ pd <> pretty "\n"
ppValue Dummy = return $ pretty "DUMMY" <> pretty "\n"