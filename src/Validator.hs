module Validator where

import AST
import Monad
import Error
import Common
import qualified Data.Vector as V
import Control.Monad (when)
import Control.Monad.Except
import GHC.Real (reduce)

-------------------------
--- Validate de NumC
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
-- Validator de probabilidades
--------------------------------
--- Un operador 
valProbDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> m ()
valProbDisc (Bin n _) _     k = when (k < 0 || k > n) (throwErrorE ProbInvalidForm)
valProbDisc (Poiss _) _     k = when (k < 0) (throwErrorE ProbInvalidForm)
valProbDisc (Geo _) _       k = when (k < 1) (throwErrorE ProbInvalidForm)
valProbDisc (Pasc r _) _    k = when (k < r) (throwErrorE ProbInvalidForm)
valProbDisc (Hiper m _ _) _ k = when (k < 0 || k > m) (throwErrorE ProbInvalidForm)
valProbDisc (Custom xs _) _ k = when (V.all (/= k) xs) (throwErrorE ProbInvalidForm)

-- Dos operadores

valProbBtDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
valProbBtDisc v Gte k1 Lte k2 = do when (k1 > k2) (throwErrorE ProbInvalidForm)
                                   valSupportDisc v k1 k2
valProbBtDisc v Gt  k1 Lte k2 = do when (k1 + 1 > k2) (throwErrorE ProbInvalidForm)
                                   valSupportDisc v (k1 + 1) k2
valProbBtDisc v Gt  k1 Lt  k2 = do when (k1 + 1 > k2 - 1) (throwErrorE ProbInvalidForm)
                                   valSupportDisc v (k1 + 1) (k2 - 1)
valProbBtDisc v Gte k1 Lt  k2 = do when (k1 > k2 - 1) (throwErrorE ProbInvalidForm)
                                   valSupportDisc v k1 (k2 - 1)
valProbBtDisc _ _ _ _ _ = throwErrorE ProbInvalidForm


valSupportDisc :: MonadProb m  => VarDisc -> Int -> Int -> m ()
valSupportDisc (Bin n _)     _ k2 = when (k2 > n) (throwErrorE ProbInvalidForm)
valSupportDisc (Pasc r _)    k1 _ = when (k1 < r) (throwErrorE ProbInvalidForm)
valSupportDisc (Hiper m _ _) _ k2 = when (k2 > m) (throwErrorE ProbInvalidForm)
valSupportDisc (Custom xs _) _ k2 = when (V.all (/= k2) xs) (throwErrorE ProbInvalidForm)
valSupportDisc _ _ _ = return ()


valProbBtCont :: MonadProb m  => VarCont -> OpComp -> Double -> OpComp -> Double -> m ()
valProbBtCont _ Gte k1 Lte k2 = when (k1 >  k2) (throwErrorE ProbInvalidForm)
valProbBtCont _ Gt k1 Lte k2  = when (k1 >= k2) (throwErrorE ProbInvalidForm)
valProbBtCont _ Gt k1 Lt k2   = when (k1 >= k2) (throwErrorE ProbInvalidForm)
valProbBtCont _ Gte k1 Lt k2  = when (k1 >= k2) (throwErrorE ProbInvalidForm)
valProbBtCont _ _ _ _ _ = throwErrorE ProbInvalidForm


------------------------------------
-- Validator de variables aleatorias
------------------------------------
valVarAle ::  MonadProb m  => RandVar -> m ()
valVarAle (Disc x) = valVarDisc x
valVarAle (Cont x) = valVarCont x


valVarDisc :: MonadProb m  => VarDisc -> m ()
valVarDisc (Bin n p) | p < 0 || p > 1 = throwErrorE InvalidProb
                     | n < 0          = throwErrorE AleInvalidForm
                     | otherwise      = return ()

valVarDisc (Poiss  l) | l < 0     = throwErrorE ProbInvalidForm
                      | otherwise = return ()

valVarDisc (Geo p) | p < 0 || p > 1 = throwErrorE InvalidProb
                   | otherwise      = return ()

valVarDisc (Pasc r p) | p < 0 || p > 1 = throwErrorE InvalidProb
                      | r < 0          = throwErrorE AleInvalidForm
                      | otherwise      = return ()

valVarDisc (Hiper m n r) | n > m     = throwErrorE AleInvalidForm
                         | r > m     = throwErrorE AleInvalidForm
                         | otherwise = return ()
valVarDisc (Custom xs ps) | V.null ps = throwErrorE AleInvalidForm
                          | V.length xs /= V.length ps = throwErrorE AleInvalidForm
                          | V.any (\p -> p < 0 || p > 1) ps = throwErrorE InvalidProb
                          | V.sum ps /= 1 = throwErrorE InvalidProb
                          | otherwise = return ()

valVarCont :: MonadProb m  => VarCont -> m ()
valVarCont (Norm m u) | u < 0     = throwErrorE AleInvalidForm
                      | otherwise = return ()
valVarCont (Unif a b) | a > b = throwErrorE AleInvalidForm
                      | otherwise = return ()
valVarCont (Expo a) | a < 0 = throwErrorE AleInvalidForm
                    | otherwise = return ()



valNode :: MonadProb m => Node -> m ()
valNode (N nd) = view nd 0 []
              where inn _ []   = False 
                    inn m (a:as)| m == a = True
                                | otherwise = inn m as
                    view []        _ _ = return ()
                    view ((n,p):s) sm names | p < 0 || p > 1 = throwErrorE InvalidProb
                                             | sm + p > 1    = throwErrorE InvalidProb
                                             | inn n names    = throwErrorE InvalidIndex
                                             | otherwise      = view s (sm + p) (n:names)
 
valMk :: MonadProb m => [Name] -> [Node] -> m ()
valMk names nodes = mapM_ (valNod valName) nodes
                              where
                                    valName n validNam  | n `elem` validNam = return ()
                                                        | otherwise         = throwErrorE InvalidIndex
                                    valNod f (N pairs) = mapM_ (\(n, _) -> f n names) pairs




makeMatriz' :: [Name] -> [Node] -> [[Double]]
makeMatriz' _ [] = []
makeMatriz' names (N n:nodes) = makerow names n : makeMatriz' names nodes
                              where
                                value _ [] = 0
                                value m ((s,d):ss) | m == s    = d
                                                   | otherwise = value m ss
                                makerow [] _ = []
                                makerow (m:ms) node = value m node : makerow ms node


makeMatriz :: [Name] -> [Node] -> Matrix Double
makeMatriz names nodes = V.fromList (map V.fromList (makeMatriz' names nodes))