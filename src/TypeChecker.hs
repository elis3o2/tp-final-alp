module TypeChecker where

import AST
import Monad
import Error
import qualified Data.Vector as V
-----------------------------
-- Helpers
-----------------------------
toDouble :: NumC -> NumC
toDouble (I n) = D (fromIntegral n)
toDouble (D n) = D n

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x :: Int)
      && x >= fromIntegral (minBound :: Int)
      && x <= fromIntegral (maxBound :: Int)

tryToInt :: NumC -> NumC
tryToInt (I n) = I n
tryToInt (D n) | isInt n   = I (round n)
               | otherwise = D n

expToDouble :: ExpNum -> ExpNum
expToDouble (Const n) = Const (toDouble n)
expToDouble e = e

tryExpToInt :: ExpNum -> ExpNum
tryExpToInt (Const n) = Const (tryToInt n)
tryExpToInt e = e


--------------------------------
-- Checker de probabilidades
--------------------------------
--- Un operador 
checkProb :: MonadProb m  => VarAle ->  NumC -> m NumC
checkProb x@(Disc v) op k = do  k' <- checkInt k
                                checkProbDisc x k'
                                return (I k')
checkProb x@(Cont v) op z = do z' <- checkDouble z
                               return (D z')

checkProbDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> m ()
checkProbDisc (Bin n _) _     k = when (k < 0 || k > n) (throwError ProbInvalidForm)
checkProbDisc (Poiss _) _     k = when (k < 0) (throwError ProbInvalidForm)
checkProbDisc (Geo _) _       k = when (k < 1) (throwError ProbInvalidForm)
checkProbDisc (Pasc r _) _    k = when (k < r) (throwError ProbInvalidForm)
checkProbDisc (Hiper m _ _) _ k = when (k < 0 || k > m) (throwError ProbInvalidForm)
checkProbDisc (Custom xs _) _ k = when (V.all (/= k) xs) (throwError ProbInvalidForm)

-- Dos operadores
checkProbBt :: MonadProb m  => VarAle -> OpComp -> NumC -> OpComp -> NumC -> m (NumC, NumC)
checkProbBt x@(Disc v) op1 k1 op2 k2 = do k1' <- checkInt k1
                                          k2' <- checkInt k2
                                          checkUpper x k1' k2'
                                          checkProbBtDisc x op1 k1' op2 k2'
                                          return (I k1', I k2')
                            
checkProbBt x@(Cont v) op1 z1 op2 z2 = do k1' <- checkDouble k1
                                          k2' <- checkDouble k2
                                          checkProbBtCont x op1 k1' op2 k2'
                                          return (D k1', D k2')


checkProbBtDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
checkProbBtDisc v Gte k1 Lte k2 = do when (k1 > k2) (throwError ProbInvalidForm)
                                     checkSupportDisc v k1 k2
checkProbBtDisc v Gt  k1 Lte k2 = do when (k1 + 1 > k2) (throwError ProbInvalidForm)
                                     checkSupportDisc v (k1 + 1) k2
checkProbBtDisc v Gt  k1 Lt  k2 = do when (k1 + 1 > k2 - 1) (throwError ProbInvalidForm)
                                     checkSupportDisc v (k1 + 1) (k2 - 1)
checkProbBtDisc v Gte k1 Lt  k2 = do when (k1 > k2 - 1) (throwError ProbInvalidForm)
                                     checkSupportDisc v k1 (k2 - 1)
checkProbBtDisc _ _ _ _ _ = throwError ProbInvalidForm


checkSupportDisc :: MonadProb m  => VarDisc -> Int -> Int -> m ()
checkSupportDisc (Bin n _)     _ k2 = when (k2 > n) (throwError ProbInvalidForm)
checkSupportDisc (Pasc r _)    k1 _ = when (k1 < r) (throwError ProbInvalidForm)
checkSupportDisc (Hiper m _ _) _ k2 = when (k2 > m) (throwError ProbInvalidForm)
checkSupportDisc (Custom xs _) _ k2 = when (V.all (/= k) xs) (throwError ProbInvalidForm)
checkSupportDisc _ _ _ = return ()


checkProbBtCont :: MonadProb m  => VarCont -> OpComp -> Double -> OpComp -> Double -> m ()
checkProbBtCont _ Gte k1 Lte k2 = when (k1 >  k2) (throwError ProbInvalidForm)
checkProbBtCont _ Gt k1 Lte k2  = when (k1 >= k2) (throwError ProbInvalidForm)
checkProbBtCont _ Gt k1 Lt k2   = when (k1 >= k2) (throwError ProbInvalidForm)
checkProbBtCont _ Gte k1 Lt k2  = when (k1 >= k2) (throwError ProbInvalidForm)
checkProbBtCont _ _ _ _ _ = throwError ProbInvalidForm


------------------------------------
-- Checker de variables aleatorias
------------------------------------
checkVarAle ::  MonadProb m  => VarAle -> m ()
checkVarAle (Disc x) = checkVarDisc
checkVarAle (Cont x) = checkVarCont


checkVarDisc :: MonadProb m  => VarDisc -> m ()
checkVarDisc (Bin n p) | p < 0 || p > 1 = throw InvalidProb
                       | n < 0 = throw AleInvalidForm
                       | otherwise      = return ()

checkVarDisc (Poiss  l) | l < 0     = throw ProbInvalidForm
                        | otherwise = return ()

checkVarDisc (Geo p) | p < 0 || p > 1 = throw InvalidProb
                     | otherwise      = return ()

checkVarDisc (Pasc r p) | p < 0 || p > 1 = throw InvalidProb
                        | r < 0          = throw AleInvalidForm
                        | otherwise      = return ()

checkVarDisc (Hiper m n r) | n > m     = throw AleInvalidForm
                           | r > m     = throw AleInvalidForm
                           | otherwise = return ()
checkVarDisc (Custom xs ps) | V.null ps = throwError AleInvalidForm
                            | V.length xs /= V.length ps = throwError AleInvalidForm
                            | V.any (\p -> p < 0 || p > 1) ps = throwError InvalidProb
                            | abs (V.sum ps - 1) > 1e-6 = throwError InvalidProb
                            | otherwise = return ()

checkVarCont :: MonadProb m  => ExpCont -> m ()
checkVarCont (Norm m u) | u < 0     = throw AleInvalidForm
                        | otherwise = return ()
checkVarCont (Unif a b) | a > b = throw AleInvalidForm
                        | otherwise = return ()
checkVarCont (Expo a) | a < 0 = throw AleInvalidForm
                      | otherwise = return ()
chackVarCont x = throw AleInvalidForm



