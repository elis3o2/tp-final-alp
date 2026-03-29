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

----
--
evalNumNum :: MonadProb m => NumExp -> m NumC
evalNumNum (ConstN n) = return n
evalNumNum (OpNum op n m) = do n' <- evalExpNum n
                            m' <- evalExpNum m
                            v  <- evalOpBin op n' m'
                            return v
evalNumNum (Minus n) = n' <- evalExpNum n
                    r  <- evalOpBin Times n' (I (-1))
                    return r  


evalVecVec :: MonadProb m => VecExp -> m (Vec NumC)
evalVecVec (ConstV v) = V.mapM evalExpNum v
evalVecVec _         = throwErrorE TypeCheckError


evalVecNum :: MonadProb m => VecExp -> m NumC
evalVecNum (Access v n) = do n' <- evalExpNum n >>= toInt
                             v' <- evalExpVec v
                             x  <- access v' n'
                             return x

evalRand :: MonadProb m => RandExp -> m RandVar
evalRand (DiscE x) = evalExpDisc x
evalRand (ContE x) = evalExpCont x


evalMarkovMk :: MonadProb m => MarkovExp -> m Markov
evalMarkovMk (MkMarkov names) = do l <- mapM (\n -> do 
                                          nd <- getNode n
                                          return nd) names 
                                   valMk names l
                                   return (Mk (V.fromList names) (makeMatriz names l))
evalMarkovMk (Stationary  c)
evalMarkovMk (NextDist  c n)



-------------------------------
-- Evaluador
--------------------------------
eval :: MonadProb m => Exp -> m Value
eval (VarRef n)       = do s <- getDecls
                        case M.lookup n s of
                           (Just v) -> return v
                           Nothing  -> throwErrorE TypeCheckError
eval (NumE x) = VNum <$> evalExpNum  x
eval (VecE x) = VNum <$> evalExpNum  x
eval (RandE x) = VNum <$> evalExpNum  x
eval (StatE x) = VNum <$> evalExpNum  x
eval (ProbE x) = VNum <$> evalExpNum  x
eval (MarkovE x) = VNum <$> evalExpNum  x
eval (NodeE x) = VNum <$> evalExpNum  x
eval (ChainE x) = VNum <$> evalExpNum  x








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



