{-|
Module      : Distribution
Description : Distribution probability functions definition
-}

module Distribution where

import Common
import CBin
import Numeric.SpecFunctions (erf)
import qualified Data.Vector as V
import Validator


-- =======================================
-- | Probability Functions
-- =======================================
-- | Discrete
functionDisc :: VarDisc -> Int -> Double
-- BINOMIAL
functionDisc (Bin n p) k = fromIntegral (toInteger n |*| toInteger k) *
                           p ^ k *
                           (1 - p) ^ (n - k)
-- POISSON
functionDisc (Poiss l) k = exp (-l) * l ^ k /
                           fromIntegral (fact (toInteger k))
-- GEOMETRIC 
functionDisc (Geo p) k = (1 - p) ^ (k - 1) * p

-- PASCAL (soporte: k >= r)
functionDisc (Pasc r p) k = fromIntegral (toInteger (k - 1) |*| toInteger (r - 1)) *
                              p ^ r *
                              (1 - p) ^ (k - r)

-- HIPERGEOMETRIC
functionDisc (Hiper m r n) k = fromIntegral (
                                   (toInteger r |*| toInteger k) *
                                   (toInteger (m - r) |*| toInteger (n - k))
                                 )
                                 /
                                 fromIntegral (toInteger m |*| toInteger n)

-- CUSTOM
functionDisc (Custom xs ps) k = ps V.! indexOf xs k

-----------------------------------------------------------------
-- | Continuous
functionCont :: VarCont -> Double -> Double 
-- NORMAL
functionCont (Norm m s) x = 1 / (s * sqrt (2 * pi)) *  
                           exp (- ((x - m)^2) / (2 * s^2))

-- EXPONENTIAL
functionCont (Expo l) x | x < 0  = 0
                        | otherwise  = l * exp (-l * x)
-- UNIFORM
functionCont (Unif a b) x | x < a || x > b = 0
                          | otherwise = 1 / (b - a)


-- ====================================================
--  Probability Query
-- ====================================================
-- | Discrete
getProbDisc :: VarDisc -> OpComp -> Int -> Double
getProbDisc var Eq  k = functionDisc var k
getProbDisc var NEq k = 1 - functionDisc var k
getProbDisc (Bin n p) Lte k = sum [functionDisc (Bin n p) i | i <- [0 .. k]]
getProbDisc (Poiss l) Lte k = sum [functionDisc (Poiss l) i | i <- [0 .. k]]
getProbDisc (Geo p) Lte k   = sum [functionDisc (Geo p) i | i <- [1 .. k]]
getProbDisc (Pasc r p) Lte k =sum [functionDisc (Pasc r p) i | i <- [r .. k]]
getProbDisc (Hiper m r n) Lte k = let lo = max 0 (n - (m - r))
                                      hi = min n r
                                  in sum [functionDisc (Hiper m r n) i | i <- [lo .. min k hi]]
-- CUSTOM
getProbDisc (Custom xs ps) Lte k = sum [p | (x,p) <- V.toList (V.zip xs ps), x <= k]

-- DERIVADAS
getProbDisc var Lt  k = getProbDisc var Lte (k - 1)
getProbDisc var Gt  k = 1 - getProbDisc var Lte k
getProbDisc var Gte k = 1 - getProbDisc var Lt k

getProbBtDisc :: VarDisc -> OpComp -> Int -> OpComp -> Int -> Double
getProbBtDisc var Gte k1 Lte k2 = sum [getProbDisc var Eq i | i <- [k1..k2]]
getProbBtDisc var Gt  k1 Lte k2 = sum [getProbDisc var Eq i | i <- [k1+1..k2]]
getProbBtDisc var Gt  k1 Lt  k2 = sum [getProbDisc var Eq i | i <- [k1+1..k2-1]]
getProbBtDisc var Gte k1 Lt  k2 = sum [getProbDisc var Eq i | i <- [k1..k2-1]]
getProbBtDisc _ _ _ _ _ = error "getProbBtDisc"

-------------------------------------------------------------
-- | Continous
getProbCont :: VarCont -> OpComp -> Double -> Double
getProbCont _ Eq  _ = 0
getProbCont _ NEq _ = 1

getProbCont (Norm  m s) Lte z = 0.5 * (1 + erf ((z - m) /
                                (s * sqrt 2)))
getProbCont (Unif a  b) Lte z | z < a     = 0
                              | z > b     = 1
                              | otherwise = (z - a) / (b - a)
getProbCont (Expo l) Lte z | z < 0     = 0
                           | otherwise = 1 - exp (-l * z)
getProbCont var Lt  z = getProbCont var Lte z
getProbCont var Gt  z = 1 - getProbCont var Lte z
getProbCont var Gte z = 1 - getProbCont var Lt z

getProbBtCont :: VarCont -> OpComp -> Double -> OpComp -> Double -> Double
getProbBtCont var _ z1 _ z2 = getProbCont var Lte z2 - getProbCont var Lte z1



-- ==========================================
-- | Stats
-- ==========================================
-- | Mean
getMean :: RandVar -> Double 
getMean (Disc x) = getMeanDisc x 
getMean (Cont x) = getMeanCont x


getMeanDisc :: VarDisc -> Double

getMeanDisc (Bin n p)   = fromIntegral n * p
getMeanDisc (Poiss l)   = l
getMeanDisc (Geo p)     = 1 / p
getMeanDisc (Pasc r p)  = fromIntegral r / p
getMeanDisc (Hiper m r n) = let m' = fromIntegral m
                                r' = fromIntegral r
                                n' = fromIntegral n
                            in n' * (r' / m')
getMeanDisc (Custom xs ps) = V.sum (V.zipWith (\x p -> fromIntegral x * p) xs ps)

getMeanCont :: VarCont -> Double
getMeanCont (Norm m _) = m
getMeanCont (Expo a) = 1 / a
getMeanCont (Unif a b) = (a + b) / 2

-------------------------------------------------
-- | Variance
getVariance :: RandVar -> Double
getVariance (Disc x) = getVarianceDisc x
getVariance (Cont x) = getVarianceCont x


getVarianceDisc :: VarDisc -> Double

getVarianceDisc (Bin n p)  = fromIntegral n * p * (1 - p)
getVarianceDisc (Poiss l)  = l
getVarianceDisc (Geo p)    = (1 - p) / p^2
getVarianceDisc (Pasc r p) = fromIntegral r * (1 - p) / p^2
getVarianceDisc (Hiper m r n) = let m' = fromIntegral m
                                    r' = fromIntegral r
                                    n' = fromIntegral n
                                    p  = r' / m'
                                in n' * p * (1 - p) * ((m' - n') / (m' - 1))
getVarianceDisc (Custom xs ps) = let xs' = V.map fromIntegral xs
                                     e   = getMeanDisc (Custom xs ps)
                                     e2  = V.sum (V.zipWith (\x p -> x * x * p) xs' ps)
                                 in e2 - e * e

getVarianceCont :: VarCont -> Double
getVarianceCont (Norm _ s) = s^2
getVarianceCont (Expo  a) = 1 / a^2
getVarianceCont (Unif  a  b) = (b - a)^2 / 12

----------------------------------------------------------
-- | Standard Deviation
getStdDev :: RandVar -> Double
getStdDev (Disc x) = sqrt (getVarianceDisc x)
getStdDev (Cont x) = sqrt (getVarianceCont x)

----------------------------------------------------------
-- | Mode
getMode :: RandVar -> Vec Double
getMode (Disc x) = V.map fromIntegral (getModeDisc x)
getMode (Cont x) = getModeCont x


getModeDisc :: VarDisc -> V.Vector Int

-- BINOMIAL
getModeDisc (Bin n p) | p == 0    = V.singleton 0
                      | p == 1    = V.singleton n
                      | isInt t   = V.fromList [m - 1, m]
                      | otherwise = V.singleton m
                      where
                        t = fromIntegral (n + 1) * p
                        m = floor t
-- POISSON
getModeDisc (Poiss l) | l <= 0    = V.singleton 0
                      | isInt l   = V.fromList [m - 1, m]
                      | otherwise = V.singleton m
                      where
                        m = floor l
-- GEOMETRIC
getModeDisc (Geo _) = V.singleton 1

-- PASCAL
getModeDisc (Pasc r p) | p == 1    = V.singleton r 
                       | otherwise = V.singleton (r + floor ((fromIntegral (r - 1) * (1 - p)) / p))

getModeDisc (Hiper m r n) = let x  = ((fromIntegral (n + 1) * fromIntegral (r + 1)) :: Double)
                                     / fromIntegral (m + 2)
                                m0 = floor x
                            in if isInt x
                                  then V.fromList [m0 - 1, m0]
                                  else V.singleton m0

getModeDisc (Custom xs ps) = let maxP = V.maximum ps
                                 eps  = 1e-9
                             in V.map fst $
                                V.filter (\(_, p) -> abs (p - maxP) < eps) $
                                V.zip xs ps

getModeCont :: VarCont -> Vec Double
getModeCont (Norm  m _) = V.singleton m
getModeCont (Expo  _) = V.singleton 0
getModeCont (Unif _ _) = V.empty

------------------------------------------------------
-- | Max Probability
getMaxP :: RandVar ->  Double
getMaxP (Disc x) = let v = getModeDisc x
                       k = fromIntegral (V.head v)
                       p = getProbDisc x Eq k
                    in p
getMaxP _ = error "Not definded"

------------------------------------------------------
-- | PDF
getPDF :: RandVar -> Double ->  Double
getPDF (Cont x) i = functionCont x i
getPDF (Disc _) _ = error "Not definded"

------------------------------------------------------
-- | Max PDF 
getMaxPDF :: RandVar ->  Double
getMaxPDF x@(Cont (Norm m _)) = getPDF x m
getMaxPDF x@(Cont (Expo _))   = getPDF x 0
getMaxPDF x@(Cont (Unif a _)) = getPDF x a
getMaxPDF (Disc _) = error "Not definded"





-- ===========================================
-- Utils
-- ===========================================
-- Utils for plot and print
nameDisc :: VarDisc -> String
nameDisc (Bin    {}) = "binomial"
nameDisc (Poiss  {}) = "poisson"
nameDisc (Geo    {}) = "geometric"
nameDisc (Pasc   {}) = "pascal"
nameDisc (Hiper  {}) = "hipergeometric"
nameDisc (Custom {}) = "custom"

nameCont :: VarCont -> String
nameCont (Norm {}) = "normal"
nameCont (Expo {}) = "exponential"
nameCont (Unif {}) = "uniform"



buildProbDisc :: VarDisc -> [(Double, Double)]
buildProbDisc x@(Bin n _) = let m = getMeanDisc x
                                s = sqrt (getVarianceDisc x)
                                a = max 0 (floor (m - 4*s))
                                b = min n (ceiling (m + 4*s))
                            in [(fromIntegral k, functionDisc x k) | k <- [a..b]]

buildProbDisc x@(Poiss _) = let m = getMeanDisc x
                                s = sqrt (getVarianceDisc x)
                                a = max 0 (floor (m - 4*s))
                                b = ceiling (m + 4*s)
                            in [(fromIntegral k, functionDisc x k) | k <- [a..b]]

buildProbDisc x@(Geo _) = let m = getMeanDisc x
                              s = sqrt (getVarianceDisc x)
                              a = max 1 (floor (m - 4*s))
                              b = ceiling (m + 4*s)
                          in [(fromIntegral k, functionDisc x k) | k <- [a..b]]

buildProbDisc x@(Pasc r _) = let m = getMeanDisc x
                                 s = sqrt (getVarianceDisc x)
                                 a = max r (floor (m - 4*s))
                                 b = ceiling (m + 4*s)
                             in [(fromIntegral k, functionDisc x k) | k <- [a..b]]

buildProbDisc x@(Hiper m r n) = let lo = max 0 (n - (m - r))
                                    hi = min n r
                                    mu = getMeanDisc x
                                    s  = sqrt (getVarianceDisc x)
                                    a  = max lo (floor (mu - 4*s))
                                    b  = min hi (ceiling (mu + 4*s))
                                in [(fromIntegral k, functionDisc x k) | k <- [a..b]]

buildProbDisc x@(Custom xs _) = [(fromIntegral k, functionDisc x k) | k <- V.toList xs]
