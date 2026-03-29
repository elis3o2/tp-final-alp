module Eval where

import AST
import Validator
import Monad
import Error
import Common
import CBin
import Lib
import qualified Data.Vector as V
import Numeric.SpecFunctions (erf)
import qualified Data.Map as M

-------------------------------------
-- Evaluadores de operaciones binarias
-------------------------------------

evalOpBin :: MonadProb m => OpBin -> NumC -> NumC -> m NumC
evalOpBin Div _ (I 0) = throwErrorE DivByZero
evalOpBin Div _ (D 0) = throwErrorE DivByZero

evalOpBin Plus  (I n) (I m) = return (I (n + m))
evalOpBin Minus (I n) (I m) = return (I (n - m))
evalOpBin Times (I n) (I m) = return (I (n * m))
evalOpBin Div   (I n) (I m) = return $ tryToInt (D (fromIntegral n / fromIntegral m))

evalOpBin Plus  (D n) (D m) = return (tryToInt (D (n + m)))
evalOpBin Minus (D n) (D m) = return (tryToInt (D (n - m)))
evalOpBin Times (D n) (D m) = return (tryToInt (D (n * m)))
evalOpBin Div   (D n) (D m) = return (tryToInt (D (n / m)))

evalOpBin op x y = evalOpBin op (makeDouble x) (makeDouble y)
                           where
                              makeDouble (I n) = D (fromIntegral n)
                              makeDouble s     = s


-------------------------------------
-- Probabilidad
-------------------------------------

getProb :: MonadProb m => RandVar -> OpComp -> NumC -> m Double
getProb (Disc x) op k = do k' <- toInt k
                           valProbDisc x op k'
                           return (getProbDisc x op k')

getProb (Cont x) op z = do z' <- toDouble z
                           return (getProbCont x op z')



getProbBt :: MonadProb m => RandVar -> OpComp -> NumC -> OpComp -> NumC -> m Double
getProbBt (Disc x) op1 k1 op2 k2 = do k1' <- toInt k1
                                      k2' <- toInt k2
                                      valProbBtDisc x op1 k1' op2 k2'
                                      return (getProbBtDisc x op1 k1' op2 k2')

getProbBt (Cont x) op1 z1 op2 z2 = do z1' <- toDouble z1
                                      z2' <- toDouble z2
                                      valProbBtCont x op1 z1' op2 z2'
                                      return (getProbBtCont x op1 z1' op2 z2')


--- Mode
getModa :: MonadProb m => RandVar -> m (Vec NumC)
getModa (Cont (Unif _ _)) = throwErrorE NotDefined
getModa (Disc x) = return $ V.map I (getModaDisc x)
getModa (Cont x) = return $ V.map D (getModaCont x)

------------------------------------
-- Máxima probabilidad
------------------------------------
getMaxP :: MonadProb m => RandVar -> m Double
getMaxP (Disc x) = let v = getModaDisc x
                       k = V.head v
                       p = getProbDisc x Eq k
                    in return p
getMaxP (Cont x) = throwErrorE DiscVarExpected


-------------------------------------
-- Función de densidad de probabilidad y su punto máximo
-------------------------------------
------ FDP
getFDP :: MonadProb m => RandVar -> Double -> m Double
getFDP (Cont (Norm m s)) x = return (1 / (s * sqrt (2 * pi)) *
                                   exp (- ((x - m)^2) / (2 * s^2)))
getFDP (Cont (Expo l)) x = if x < 0 then
                              return 0 else return (l * exp (-l * x))
getFDP (Cont (Unif a b)) x = if x < a || x > b
                             then return 0 else
                             return (1 / (b - a))
getFDP (Disc _) _ = throwErrorE DiscVarExpected
------ MaxFDP 

getMaxFDP :: MonadProb m => RandVar -> m Double
getMaxFDP x@(Cont (Norm m _)) = getFDP x m
getMaxFDP x@(Cont (Expo _))   = getFDP x 0
getMaxFDP x@(Cont (Unif a _)) = getFDP x a
getMaxFDP (Disc _) = throwErrorE ContVarExpected
-------------------------------------
-- Funciones sobre vectores 
-------------------------------------
access :: MonadProb m => Vec NumC -> Int -> m NumC
access v i | i < 0 || i > V.length v - 1 = throwErrorE InvalidIndex
           | otherwise = return (v V.! i)


-------------------------------------
-- Evaluación de expresiones Numericas
-------------------------------------
evalExpNum :: MonadProb m => Exp -> m NumC
evalExpNum (VarRef v)    = getNumC v
evalExpNum (ConstN n) = return n
evalExpNum (OpNum op n m) = do n' <- evalExpNum n
                               m' <- evalExpNum m
                               v  <- evalOpBin op n' m'
                               return v
evalExpNum (Prob v op k) = do v' <- evalExpAle v
                             k' <- evalExpNum k
                             p  <- getProb v' op k'
                             return (D p)

evalExpNum s@(ProbBetween v op1 k1 op2 k2) = do v'  <- evalExpAle v
                                          k1' <- evalExpNum k1
                                          k2' <- evalExpNum k2
                                          p   <- getProbBt v' op1 k1' op2 k2'
                                          return (D p)

evalExpNum (Access v n) = do n' <- evalExpNum n >>= toInt
                             v' <- evalExpVec v
                             x  <- access v' n'
                             return x

evalExpNum (Mean v) = do v' <- evalExpAle v
                        return (D (getEsp v'))

evalExpNum (Variance v) = do v' <- evalExpAle v
                         return (D (getVari v'))

evalExpNum (StdDev v) = do v' <- evalExpAle v
                         return (D (getDesv v'))

evalExpNum (MaxP v) = do v' <- evalExpAle v
                         k  <- getMaxP v'
                         return (D k)

evalExpNum (FDP v x) = do v' <- evalExpAle v
                          x' <- evalExpNum x >>= toDouble
                          d  <- getFDP v' x'
                          return (D d)

evalExpNum (MaxFDP v) = do v' <- evalExpAle v
                           d  <- getMaxFDP v'
                           return (D d)
evalExpNum  _ = throwErrorE TypeCheckError



--------------------------------
--- Evaluador de Vectores
--------------------------------

evalExpVec :: MonadProb m => Exp -> m (Vec NumC)
evalExpVec (VarRef x)    = getVec x
evalExpVec (ConstV v) = V.mapM evalExpNum v
evalExpVec (Mode x)   = do x' <- evalExpAle x
                           m <- getModa x'
                           return m
evalExpVec _         = throwErrorE TypeCheckError


-------------------------------------
-- Evaluador de Variables Aleatorias
-------------------------------------

evalExpAle :: MonadProb m => Exp -> m RandVar
evalExpAle (VarRef x)         = getAle x 
evalExpAle (Rand (DiscE x)) = Disc <$> evalExpDisc x
evalExpAle (Rand (ContE x)) = Cont <$> evalExpCont x
evalExpAle _ = throwErrorE TypeCheckError


evalExpDisc :: MonadProb m => ExpDisc -> m VarDisc
evalExpDisc (BinE n p) = do n' <- evalExpNum n >>= toInt
                            p' <- evalExpNum p >>= toDouble
                            return (Bin n' p')

evalExpDisc (PoissE l) = do l' <- evalExpNum l >>= toDouble
                            return (Poiss l')

evalExpDisc (GeoE p) = do p' <- evalExpNum p >>= toDouble
                          return (Geo p')

evalExpDisc (PascE r p) = do r' <- evalExpNum r >>= toInt
                             p' <- evalExpNum p >>= toDouble
                             return (Pasc r' p')

evalExpDisc (HiperE m r n) = do m' <- evalExpNum m >>= toInt
                                r' <- evalExpNum r >>= toInt
                                n' <- evalExpNum n >>= toInt
                                return (Hiper m' r' n')

evalExpDisc (CustomE v p) = do v' <- evalExpVec v >>= V.mapM toInt
                               p' <- evalExpVec p >>= V.mapM toDouble
                               return (Custom v' p')

evalExpCont :: MonadProb m => ExpCont -> m VarCont
evalExpCont (NormE m s) = do m' <- evalExpNum m >>= toDouble
                             s' <- evalExpNum s >>= toDouble
                             return (Norm m' s')

evalExpCont (UnifE a b) = do a' <- evalExpNum a >>= toDouble
                             b' <- evalExpNum b >>= toDouble
                             return (Unif a' b')

evalExpCont (ExpoE a) = do a' <- evalExpNum a >>= toDouble
                           return (Expo a')




evalExpNode :: MonadProb m => Exp -> m Node
evalExpNode (Node l) = do
  l' <- mapM (\(n,e) -> do
                v <- evalExpNum e
                d <- toDouble v
                return (n, d)) l
  valNode (N l')
  return (N l')
evalExpNode _ = throwErrorE InvalidVarType


evalExpMk :: MonadProb m => Exp -> m Markov
evalExpMk (MkE names) = do l <- mapM (\n -> do 
                                          nd <- getNode n
                                          return nd) names 
                           valMk names l
                           return (Mk (V.fromList names) (makeMatriz names l)) 
evalExpMk _ = throwErrorE VarNotScoope                       



-------------------------------
-- Evaluador
--------------------------------
eval :: MonadProb m => Exp -> m Value
eval (VarRef n)       = do s <- getDecls
                        case M.lookup n s of
                           (Just v) -> return v
                           Nothing  -> throwErrorE TypeCheckError
eval x@(ConstN {}) = VNum <$> evalExpNum  x
eval x@(UMinus {}) = VNum <$> evalExpNum  x
eval x@(OpNum  {}) = VNum <$> evalExpNum  x
eval x@(Access {}) = VNum <$> evalExpNum  x
eval x@(Mean    {}) = VNum <$> evalExpNum  x
eval x@(Variance   {}) = VNum <$> evalExpNum  x
eval x@(StdDev   {}) = VNum <$> evalExpNum  x
eval x@(FDP    {}) = VNum <$> evalExpNum  x
eval x@(MaxP   {}) = VNum <$> evalExpNum  x
eval x@(MaxFDP {}) = VNum <$> evalExpNum  x
eval x@(Prob    {}) = VNum <$> evalExpNum  x
eval x@(ProbBetween  {}) = VNum <$> evalExpNum  x
eval x@(Mode   {}) = VVec <$> evalExpVec  x
eval x@(ConstV {}) = VVec <$> evalExpVec  x
eval x@(Rand    {}) = VAle <$> evalExpAle  x
eval x@(Node   {}) = VN   <$> evalExpNode x
eval x@(MkE  {}) = VMk  <$> evalExpMk   x