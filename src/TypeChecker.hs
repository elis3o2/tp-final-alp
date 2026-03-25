module TypeChecker where

import AST
import Monad
import Error
import Common
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.Except

-------------------------
--- Checker de NumC
-------------------------

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x :: Int)
      && x >= fromIntegral (minBound :: Int)
      && x <= fromIntegral (maxBound :: Int)


toInt :: MonadProb m => NumC -> m Int
toInt (I n) = return n
toInt (D n) | isInt n   = return (round n)
            | otherwise = throwErrorE IntValueExpected

toDouble :: MonadProb m => NumC -> m Double
toDouble (I n) = return (fromIntegral n)
toDouble (D n) = return n


tryToInt :: NumC -> NumC
tryToInt (D n) | isInt n   = I (round n)
               | otherwise = D n
tryToInt x = x

--------------------------------
-- Checker de probabilidades
--------------------------------
--- Un operador 

checkProbDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> m ()
checkProbDisc (Bin n _) _     k = when (k < 0 || k > n) (throwErrorE ProbInvalidForm)
checkProbDisc (Poiss _) _     k = when (k < 0) (throwErrorE ProbInvalidForm)
checkProbDisc (Geo _) _       k = when (k < 1) (throwErrorE ProbInvalidForm)
checkProbDisc (Pasc r _) _    k = when (k < r) (throwErrorE ProbInvalidForm)
checkProbDisc (Hiper m _ _) _ k = when (k < 0 || k > m) (throwErrorE ProbInvalidForm)
checkProbDisc (Custom xs _) _ k = when (V.all (/= k) xs) (throwErrorE ProbInvalidForm)

-- Dos operadores

checkProbBtDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
checkProbBtDisc v Gte k1 Lte k2 = do when (k1 > k2) (throwErrorE ProbInvalidForm)
                                     checkSupportDisc v k1 k2
checkProbBtDisc v Gt  k1 Lte k2 = do when (k1 + 1 > k2) (throwErrorE ProbInvalidForm)
                                     checkSupportDisc v (k1 + 1) k2
checkProbBtDisc v Gt  k1 Lt  k2 = do when (k1 + 1 > k2 - 1) (throwErrorE ProbInvalidForm)
                                     checkSupportDisc v (k1 + 1) (k2 - 1)
checkProbBtDisc v Gte k1 Lt  k2 = do when (k1 > k2 - 1) (throwErrorE ProbInvalidForm)
                                     checkSupportDisc v k1 (k2 - 1)
checkProbBtDisc _ _ _ _ _ = throwErrorE ProbInvalidForm


checkSupportDisc :: MonadProb m  => VarDisc -> Int -> Int -> m ()
checkSupportDisc (Bin n _)     _ k2 = when (k2 > n) (throwErrorE ProbInvalidForm)
checkSupportDisc (Pasc r _)    k1 _ = when (k1 < r) (throwErrorE ProbInvalidForm)
checkSupportDisc (Hiper m _ _) _ k2 = when (k2 > m) (throwErrorE ProbInvalidForm)
checkSupportDisc (Custom xs _) _ k2 = when (V.all (/= k2) xs) (throwErrorE ProbInvalidForm)
checkSupportDisc _ _ _ = return ()


checkProbBtCont :: MonadProb m  => VarCont -> OpComp -> Double -> OpComp -> Double -> m ()
checkProbBtCont _ Gte k1 Lte k2 = when (k1 >  k2) (throwErrorE ProbInvalidForm)
checkProbBtCont _ Gt k1 Lte k2  = when (k1 >= k2) (throwErrorE ProbInvalidForm)
checkProbBtCont _ Gt k1 Lt k2   = when (k1 >= k2) (throwErrorE ProbInvalidForm)
checkProbBtCont _ Gte k1 Lt k2  = when (k1 >= k2) (throwErrorE ProbInvalidForm)
checkProbBtCont _ _ _ _ _ = throwErrorE ProbInvalidForm


------------------------------------
-- Checker de variables aleatorias
------------------------------------
checkVarAle ::  MonadProb m  => VarAle -> m ()
checkVarAle (Disc x) = checkVarDisc x
checkVarAle (Cont x) = checkVarCont x


checkVarDisc :: MonadProb m  => VarDisc -> m ()
checkVarDisc (Bin n p) | p < 0 || p > 1 = throwErrorE InvalidProb
                       | n < 0 = throwErrorE AleInvalidForm
                       | otherwise      = return ()

checkVarDisc (Poiss  l) | l < 0     = throwErrorE ProbInvalidForm
                        | otherwise = return ()

checkVarDisc (Geo p) | p < 0 || p > 1 = throwErrorE InvalidProb
                     | otherwise      = return ()

checkVarDisc (Pasc r p) | p < 0 || p > 1 = throwErrorE InvalidProb
                        | r < 0          = throwErrorE AleInvalidForm
                        | otherwise      = return ()

checkVarDisc (Hiper m n r) | n > m     = throwErrorE AleInvalidForm
                           | r > m     = throwErrorE AleInvalidForm
                           | otherwise = return ()
checkVarDisc (Custom xs ps) | V.null ps = throwErrorE AleInvalidForm
                            | V.length xs /= V.length ps = throwErrorE AleInvalidForm
                            | V.any (\p -> p < 0 || p > 1) ps = throwErrorE InvalidProb
                            | abs (V.sum ps - 1) > 1e-6 = throwErrorE InvalidProb
                            | otherwise = return ()

checkVarCont :: MonadProb m  => VarCont -> m ()
checkVarCont (Norm m u) | u < 0     = throwErrorE AleInvalidForm
                        | otherwise = return ()
checkVarCont (Unif a b) | a > b = throwErrorE AleInvalidForm
                        | otherwise = return ()
checkVarCont (Expo a) | a < 0 = throwErrorE AleInvalidForm
                      | otherwise = return ()



