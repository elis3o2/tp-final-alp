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
import Matrix
import Validator (valMkName, toInt)
import Random
-------------------------------------
-- Evaluadores de operaciones binarias
-------------------------------------

evalOpBin :: MonadProb m => OpBin -> Double -> Double -> m Double
evalOpBin Div _ 0 = throwErrorE DivByZero
evalOpBin Plus  n m = return (n + m)
evalOpBin Minus n m = return (n - m)
evalOpBin Times n m = return (n * m)
evalOpBin Div   n m = return (n / m)

-------------------------------------
-- Probabilidad
-------------------------------------

getProb :: MonadProb m => RandVar -> OpComp -> Double -> m Double
getProb (Disc x) op k = do k' <- toInt k
                           valProbDisc x op k'
                           return (getProbDisc x op k')

getProb (Cont x) op z = do return (getProbCont x op z)



getProbBt :: MonadProb m => RandVar -> OpComp -> Double -> OpComp -> Double -> m Double
getProbBt (Disc x) op1 k1 op2 k2 = do k1' <- toInt k1
                                      k2' <- toInt k2
                                      valProbBtDisc x op1 k1' op2 k2'
                                      return (getProbBtDisc x op1 k1' op2 k2')

getProbBt (Cont x) op1 z1 op2 z2 = do valProbBtCont x op1 z1 op2 z2
                                      return (getProbBtCont x op1 z1 op2 z2)



-------------------------------------
-- Funciones sobre vectores 
-------------------------------------
access :: MonadProb m => Vec Double -> Int -> m Double
access v i | i < 0 || i > V.length v - 1 = throwErrorE InvalidIndex
           | otherwise = return (v V.! i)


-------------------------------------
-- Evaluación de expresiones Numericas
-------------------------------------
evalNumExp :: MonadProb m => Exp -> m Double
evalNumExp (VarRef v)    = getNumC v
evalNumExp (ConstN n) = return n
evalNumExp (OpNum op n m) = do n' <- evalNumExp n
                               m' <- evalNumExp m
                               v  <- evalOpBin op n' m'
                               return v
evalNumExp (Prob v op k) = do v' <- evalRandExp v
                              k' <- evalNumExp k
                              p  <- getProb v' op k'
                              return p

evalNumExp (ProbBetween v op1 k1 op2 k2) = do v'  <- evalRandExp v
                                              k1' <- evalNumExp k1
                                              k2' <- evalNumExp k2
                                              p   <- getProbBt v' op1 k1' op2 k2'
                                              return p

evalNumExp (Access v n) = do n' <- evalNumExp n >>= toInt
                             v' <- evalVecExp v
                             x  <- access v' n'
                             return x

evalNumExp (Mean v) = do v' <- evalRandExp v
                         return (getEsp v')

evalNumExp (Variance v) = do v' <- evalRandExp v
                             return (getVari v')

evalNumExp (StdDev v) = do v' <- evalRandExp v
                           return (getDesv v')

evalNumExp (MaxP v) = do v' <- evalRandExp v
                         return (getMaxP v')

evalNumExp (FDP v x) = do v' <- evalRandExp v
                          x' <- evalNumExp x 
                          return (getFDP v' x')

evalNumExp (MaxFDP v) = do v' <- evalRandExp v
                           return (getMaxFDP v')

evalNumExp (ProbStep x i j n) = do x' <- evalMarkovExp x
                                   n' <- evalNumExp n >>= toInt
                                   valMkName x' i
                                   valMkName x' j 
                                   return (getProbSteps x' i j n')
evalNumExp (ProbPath x c)     = do x' <- evalMarkovExp x 
                                   c' <- evalPathExp c 
                                   valMkPath x' c'
                                   return (getProbPath x' c')
evalNumExp (ProbHit x i j)    = do x' <- evalMarkovExp x
                                   valMkName x' i
                                   valMkName x' j                                   
                                   return (getProbHit x' i j)


evalNumExp  _ = throwErrorE TypeCheckError



--------------------------------
--- Evaluador de Vectores
--------------------------------

evalVecExp :: MonadProb m => Exp -> m (Vec Double)
evalVecExp (VarRef x) = getVec x
evalVecExp (ConstV v) = V.mapM evalNumExp v
evalVecExp (Mode x)   = do x' <- evalRandExp x
                           return (getMode x')
evalVecExp (Stationary x) = do x' <- evalMarkovExp x
                               return (getStationary x')
evalVecExp _         = throwErrorE TypeCheckError


-------------------------------------
-- Evaluador de Variables Aleatorias
-------------------------------------

evalRandExp :: MonadProb m => Exp -> m RandVar
evalRandExp (VarRef x)         = getAle x
evalRandExp (Rand (DiscE x)) = Disc <$> evalExpDisc x
evalRandExp (Rand (ContE x)) = Cont <$> evalExpCont x
evalRandExp _ = throwErrorE TypeCheckError


evalExpDisc :: MonadProb m => ExpDisc -> m VarDisc
evalExpDisc (BinE n p) = do n' <- evalNumExp n >>= toInt
                            p' <- evalNumExp p 
                            return (Bin n' p')

evalExpDisc (PoissE l) = do l' <- evalNumExp l 
                            return (Poiss l')

evalExpDisc (GeoE p) = do p' <- evalNumExp p 
                          return (Geo p')

evalExpDisc (PascE r p) = do r' <- evalNumExp r >>= toInt
                             p' <- evalNumExp p 
                             return (Pasc r' p')

evalExpDisc (HiperE m r n) = do m' <- evalNumExp m >>= toInt
                                r' <- evalNumExp r >>= toInt
                                n' <- evalNumExp n >>= toInt
                                return (Hiper m' r' n')

evalExpDisc (CustomE v p) = do v' <- evalVecExp v >>= V.mapM toInt
                               p' <- evalVecExp p
                               return (Custom v' p')

evalExpCont :: MonadProb m => ExpCont -> m VarCont
evalExpCont (NormE m s) = do m' <- evalNumExp m 
                             s' <- evalNumExp s 
                             return (Norm m' s')

evalExpCont (UnifE a b) = do a' <- evalNumExp a 
                             b' <- evalNumExp b 
                             return (Unif a' b')

evalExpCont (ExpoE a) = do a' <- evalNumExp a 
                           return (Expo a')




evalNodeExp :: MonadProb m => NodeExp -> m NodeVal
evalNodeExp (NE l) = do
  l' <- mapM (\(n,e) -> do
                v <- evalNumExp e
                return (n, v)) l
  valNode (N l')
  return (N l')


evalPathExp :: MonadProb m => Exp -> m Path
evalPathExp (VarRef x)            = getPath x
evalPathExp (ConstCh x)           = return x
evalPathExp (SimulFromName x m s) = do x' <- evalMarkovExp x 
                                        valMkName x' m 
                                        s' <- evalNumExp s >>= toInt
                                        c  <- doSimulFromName x' m s'
                                        return c  
evalPathExp (SimulFromVec x v s)  = do x' <- evalMarkovExp x 
                                        v' <- evalVecExp v 
                                        valMkVector x' v'
                                        s' <- evalNumExp s >>= toInt
                                        c  <- doSimulFromVec x' v' s'
                                        return c 
evalPathExp _ = throwErrorE TypeCheckError

evalMarkov :: MonadProb m => MarkovExp -> m Markov
evalMarkov (MarkovE names) = let lnames = V.toList names in
                             do nodes <- mapM getNode lnames
                                valMarkov lnames nodes
                                return (Mk names (makeMatriz lnames nodes))



evalMarkovExp :: MonadProb m => Exp -> m Markov
evalMarkovExp (VarRef n) = getMarkov n
evalMarkovExp (Markov x) = evalMarkov x
evalMarkovExp (NextDist x n) = do x' <- evalMarkovExp x 
                                  n' <- evalNumExp n >>= toInt
                                  let nam = getNames x' 
                                      mat = getMatrix x' in 
                                   return (Mk nam (matrixEx mat n'))
evalMarkovExp _ = throwErrorE TypeCheckError
-------------------------------
-- Evaluador
--------------------------------
eval :: MonadProb m => Exp -> m Value
eval (VarRef n)    = do s <- getDecls
                        case M.lookup n s of
                             (Just v) -> return (fst v)
                             Nothing  -> throwErrorE TypeCheckError
eval x@(ConstN        {}) = VNum   <$> evalNumExp    x
eval x@(UMinus        {}) = VNum   <$> evalNumExp    x
eval x@(OpNum         {}) = VNum   <$> evalNumExp    x
eval x@(Access        {}) = VNum   <$> evalNumExp    x
eval x@(Mean          {}) = VNum   <$> evalNumExp    x
eval x@(Variance      {}) = VNum   <$> evalNumExp    x
eval x@(StdDev        {}) = VNum   <$> evalNumExp    x
eval x@(FDP           {}) = VNum   <$> evalNumExp    x
eval x@(MaxP          {}) = VNum   <$> evalNumExp    x
eval x@(MaxFDP        {}) = VNum   <$> evalNumExp    x
eval x@(Prob          {}) = VNum   <$> evalNumExp    x
eval x@(ProbBetween   {}) = VNum   <$> evalNumExp    x
eval x@(Mode          {}) = VVec   <$> evalVecExp    x
eval x@(ConstV        {}) = VVec   <$> evalVecExp    x
eval x@(Rand          {}) = VRand  <$> evalRandExp   x
eval x@(ConstCh       {}) = VPath <$> evalPathExp  x
eval x@(Markov        {}) = VMark  <$> evalMarkovExp x
eval x@(ProbStep      {}) = VNum   <$> evalNumExp    x
eval x@(ProbPath      {}) = VNum   <$> evalNumExp    x
eval x@(ProbHit       {}) = VNum   <$> evalNumExp    x
eval x@(NextDist      {}) = VMark  <$> evalMarkovExp x
eval x@(Stationary    {}) = VVec   <$> evalVecExp    x
eval x@(SimulFromName {}) = VPath <$> evalPathExp  x
eval x@(SimulFromVec  {}) = VPath <$> evalPathExp  x