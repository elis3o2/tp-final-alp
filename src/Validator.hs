module Validator where
import Monad
import Error
import Common
import qualified Data.Vector as V
import qualified Data.Set as S
import Control.Monad.Except

-- =================================
-- | Validate Num
-- =================================
isInt :: Double -> Bool
isInt x = x == fromIntegral (round x :: Int)
      && x >= fromIntegral (minBound :: Int)
      && x <= fromIntegral (maxBound :: Int)


toInt :: MonadProb m => Double -> m Int
toInt n | isInt n   = return (round n)
        | otherwise = throwErrorE IntValueExpected



-- ======================================
-- Probability Validator
-- ======================================
-- One operator 
valProbDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> m ()
valProbDisc (Bin n _) _     k = when (k < 0 || k > n) (throwErrorE InvalidRanges)
valProbDisc (Poiss _) _     k = when (k < 0) (throwErrorE InvalidRanges)
valProbDisc (Geo _) _       k = when (k < 1) (throwErrorE InvalidRanges)
valProbDisc (Pasc r _) _    k = when (k < r) (throwErrorE InvalidRanges)
valProbDisc (Hiper m r n) _ k = when (k < max 0 (n - (m - r)) || k > min n r) (throwErrorE InvalidRanges)
valProbDisc (Custom xs _) _ k = when (V.all (/= k) xs) (throwErrorE InvalidRanges)

-- Two operators
valProbBtDisc :: MonadProb m  => VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
valProbBtDisc v Gte k1 Lte k2 = do when (k1 > k2) (throwErrorE InvalidRanges)
                                   valSupportDisc v k1 k2
valProbBtDisc v Gt  k1 Lte k2 = do when (k1 + 1 > k2) (throwErrorE InvalidRanges)
                                   valSupportDisc v (k1 + 1) k2
valProbBtDisc v Gt  k1 Lt  k2 = do when (k1 + 1 > k2 - 1) (throwErrorE InvalidRanges)
                                   valSupportDisc v (k1 + 1) (k2 - 1)
valProbBtDisc v Gte k1 Lt  k2 = do when (k1 > k2 - 1) (throwErrorE InvalidRanges)
                                   valSupportDisc v k1 (k2 - 1)
valProbBtDisc _ _ _ _ _ = throwErrorE InvalidRanges


valSupportDisc :: MonadProb m  => VarDisc -> Int -> Int -> m ()
valSupportDisc (Bin n _) k1 k2 = when (k1 < 0 || k2 > n) (throwErrorE InvalidRanges)
valSupportDisc (Pasc r _) k1 _ = when (k1 < r) (throwErrorE InvalidRanges)
valSupportDisc (Hiper m r n) k1 k2 = let lo = max 0 (n - (m - r))
                                         hi = min n r
                                 in when (k1 < lo || k2 > hi)(throwErrorE InvalidRanges)
valSupportDisc (Custom xs _) k1 k2 = when (V.all (\x -> x < k1 || x > k2) xs)
                                    (throwErrorE InvalidRanges)
valSupportDisc _ _ _ = return ()


valProbBtCont :: MonadProb m  => VarCont -> OpComp -> Double -> OpComp -> Double -> m ()
valProbBtCont _ Gte k1 Lte k2 = when (k1 >  k2) (throwErrorE InvalidRanges)
valProbBtCont _ Gt k1 Lte k2  = when (k1 >= k2) (throwErrorE InvalidRanges)
valProbBtCont _ Gt k1 Lt k2   = when (k1 >= k2) (throwErrorE InvalidRanges)
valProbBtCont _ Gte k1 Lt k2  = when (k1 >= k2) (throwErrorE InvalidRanges)
valProbBtCont _ _ _ _ _ = throwErrorE InvalidRanges


-- ===========================================
-- | Random Variables Validator
-- ============================================
valVarRand ::  MonadProb m  => RandVar -> m ()
valVarRand (Disc x) = valVarDisc x
valVarRand (Cont x) = valVarCont x

-- | Discrete
valVarDisc :: MonadProb m  => VarDisc -> m ()
valVarDisc (Bin n p) | p < 0 || p > 1 = throwErrorE InvalidProb
                     | n < 0          = throwErrorE RandInvalidForm
                     | otherwise      = return ()

valVarDisc (Poiss  l) | l < 0     = throwErrorE RandInvalidForm
                      | otherwise = return ()

valVarDisc (Geo p) | p <= 0 || p > 1 = throwErrorE InvalidProb
                   | otherwise       = return ()

valVarDisc (Pasc r p) | p <= 0 || p > 1 = throwErrorE InvalidProb
                      | r < 0           = throwErrorE RandInvalidForm
                      | otherwise       = return ()

valVarDisc (Hiper m r n) | m <= 0     = throwErrorE RandInvalidForm
                         | r < 0      = throwErrorE RandInvalidForm
                         | n < 0      = throwErrorE RandInvalidForm
                         | r > m      = throwErrorE RandInvalidForm
                         | n > m      = throwErrorE RandInvalidForm
                         | otherwise  = return ()

valVarDisc (Custom xs ps) | V.null ps = throwErrorE RandInvalidForm
                          | S.size (S.fromList (V.toList xs)) /= V.length xs = throwErrorE RandInvalidForm
                          | V.any (\p -> p < 0 || p > 1) ps = throwErrorE InvalidProb
                          | abs (V.sum ps - 1) > 1e-9 = throwErrorE InvalidProb
                          | otherwise = return ()

-- | Continous
valVarCont :: MonadProb m  => VarCont -> m ()
valVarCont (Norm m u) | u < 0     = throwErrorE RandInvalidForm
                      | otherwise = return ()
valVarCont (Unif a b) | a > b = throwErrorE RandInvalidForm
                      | otherwise = return ()
valVarCont (Expo a) | a < 0 = throwErrorE RandInvalidForm
                    | otherwise = return ()


-- =========================================
-- | Markov Validator
-- ==========================================
-- | Main
valMarkov :: MonadProb m => [Name] -> [NodeVal] -> m ()
valMarkov names nodes = mapM_ (valNod valName) nodes
                              where
                                    valName n validNam  | n `elem` validNam = return ()
                                                        | otherwise         = throwErrorE InvalidName
                                    valNod f (N pairs) = mapM_ (\(n, _) -> f n names) pairs
-- | Vector
valMkVector :: MonadProb m => Markov -> Vec Double -> m ()
valMkVector (Mk n _) v | V.length n /= V.length v  = throwErrorE InvalidVector
                       | V.any (\p -> p < 0 || p > 1) v = throwErrorE InvalidProb
                       | V.sum v /= 1 = throwErrorE InvalidProb
                       | otherwise = return ()

-- | Nodes
valNode :: MonadProb m => NodeVal -> m ()
valNode (N nd) = view nd 0 []
              where inn _ []   = False 
                    inn m (a:as)| m == a = True
                                | otherwise = inn m as
                    view []        _ _ = return ()
                    view ((n,p):s) sm names  | p < 0 || p > 1 = throwErrorE InvalidProb
                                             | sm + p > 1     = throwErrorE InvalidProb
                                             | inn n names    = throwErrorE InvalidName
                                             | otherwise      = view s (sm + p) (n:names)
 
-- | Names
valMkName :: MonadProb m => Markov -> Name -> m ()
valMkName (Mk names _) n | V.elem n names = return ()
                         | otherwise      = throwErrorE InvalidName

-- | Path
valMkPath :: MonadProb m => Markov -> Path -> m ()
valMkPath x n | V.length n < 2 = throwErrorE InvalidSteps
              | otherwise = do _ <- V.mapM (\name -> valMkName x name) n
                               return ()


-- | Steps
valSteps :: MonadProb m => Int -> m ()
valSteps n | n <= 0 = throwErrorE InvalidSteps
           | otherwise = return ()