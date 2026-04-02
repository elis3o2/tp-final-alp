module Lib where

import AST
import Common
import Matrix
import qualified Data.Vector as V
import Validator
import CBin
import Numeric.SpecFunctions (erf)

------ Discretas
getProbDisc :: VarDisc -> OpComp -> Int -> Double

-- BINOMIAL
getProbDisc (Bin n p) Eq k = fromIntegral (
                              (toInteger n) |*| (toInteger k)
                              ) *
                              p ^ k *
                              (1 - p) ^ (n - k)
-- POISSON
getProbDisc (Poiss l) Eq k = exp (-l) * l ^ k /
                           fromIntegral (fact (toInteger k))

-- GEOMÉTRICA 
getProbDisc (Geo p) Eq k = (1 - p) ^ (k - 1) * p

-- PASCAL 
getProbDisc (Pasc r p) Eq k = fromIntegral (
                                 (toInteger (k - 1)) |*| (toInteger (r - 1))
                              ) *
                                 p ^ r *(1 - p) ^ (k - r)
-- HIPERGEOMÉTRICA
getProbDisc (Hiper m r n) Eq k  = fromIntegral (
                                    ((toInteger r) |*| (toInteger k)) *
                                    ((toInteger (m - r)) |*| (toInteger (n - k)))
                                 )
                                 /
                                 fromIntegral (
                                   (toInteger m) |*| (toInteger n)
                                 )
-- CUSTOM
getProbDisc (Custom v1 v2) Eq k = v2 V.! indexOf v1 k


getProbDisc var Lte k = sum [getProbDisc var Eq i | i <- [0..k]]
getProbDisc var Lt  k = sum [getProbDisc var Eq i | i <- [0..k-1]]
getProbDisc var Gt  k = 1 - getProbDisc var Lte k
getProbDisc var Gte k = 1 - getProbDisc var Lt k
getProbDisc _ _ _ = error "getProbDisc"

getProbBtDisc :: VarDisc -> OpComp -> Int -> OpComp -> Int -> Double
getProbBtDisc var Gte k1 Lte k2 = sum [getProbDisc var Eq i | i <- [k1..k2]]
getProbBtDisc var Gt  k1 Lte k2 = sum [getProbDisc var Eq i | i <- [k1+1..k2]]
getProbBtDisc var Gt  k1 Lt  k2 = sum [getProbDisc var Eq i | i <- [k1+1..k2-1]]
getProbBtDisc var Gte k1 Lt  k2 = sum [getProbDisc var Eq i | i <- [k1..k2-1]]
getProbBtDisc _ _ _ _ _ = error "getProbBtDisc"


------- Continuas
getProbCont :: VarCont -> OpComp -> Double -> Double
getProbCont _ Eq  _ = 0
getProbCont _ NEq _ = 1

-- NORMAL
getProbCont (Norm  m s) Lte z = 0.5 * (1 + erf ((z - m) /
                                (s * sqrt 2)))

-- UNIFORME
getProbCont (Unif a  b) Lte z | z < a     = 0
                              | z > b     = 1
                              | otherwise = (z - a) / (b - a)

-- EXPONENCIAL
getProbCont (Expo l) Lte z | z < 0     = 0
                           | otherwise = 1 - exp (-l * z)

getProbCont var Lt  z = getProbCont var Lte z
getProbCont var Gt  z = 1 - getProbCont var Lte z
getProbCont var Gte z = 1 - getProbCont var Lt z

getProbBtCont :: VarCont -> OpComp -> Double -> OpComp -> Double -> Double
getProbBtCont var _ z1 _ z2 = getProbCont var Lte z2 - getProbCont var Lte z1




----------------------------------------
-- Esperanza / Varianza / Desvío / Mode
----------------------------------------
----- Esperanza
getEsp ::  RandVar -> Double
getEsp (Disc x) = getEspDisc x
getEsp (Cont x) = getEspCont x

getEspDisc :: VarDisc -> Double
getEspDisc (Bin  n p) = fromIntegral n * p
getEspDisc (Poiss l) = l
getEspDisc (Geo p) = 1 / p
getEspDisc (Pasc r p) = fromIntegral r / p
getEspDisc (Hiper m n r) = let m' = fromIntegral m
                               n' = fromIntegral n
                               r' = fromIntegral r
                            in r' * (n' / m')
getEspDisc (Custom xs ps) = V.sum (V.zipWith (\x p -> fromIntegral x * p) xs ps)


getEspCont :: VarCont -> Double
getEspCont (Norm m u) = m
getEspCont (Expo a) = 1 / a
getEspCont (Unif a b) = (a + b) / 2

----- Varianza
getVari :: RandVar -> Double
getVari (Disc x) = getVariDisc x
getVari (Cont x) = getVariCont x

getVariDisc :: VarDisc -> Double
getVariDisc (Bin n p) = fromIntegral n * p * (1 - p)
getVariDisc (Poiss l) = l
getVariDisc (Geo p) = (1 - p) / (p * p)
getVariDisc (Pasc r p) = fromIntegral r * (1 - p) / (p * p)
getVariDisc (Hiper m n r) = let m' = fromIntegral m
                                n' = fromIntegral n
                                r' = fromIntegral r
                            in r' * (n' / m') * (1 - n' / m') * ((m' - r') / (m' - 1))
getVariDisc (Custom xs ps) = let xs' = V.map fromIntegral xs
                                 esp =  getEspDisc (Custom xs ps)
                                 esp2 = V.sum (V.zipWith (\x p -> x * x * p) xs' ps)
                            in esp2 - esp * esp

getVariCont :: VarCont -> Double
getVariCont (Norm m s) = s^2
getVariCont (Expo  a) = 1 / a^2
getVariCont (Unif  a  b) = (b - a)^2 / 12


---- Desvio Estandar
getDesv :: RandVar -> Double
getDesv (Disc x) = sqrt (getVariDisc x)
getDesv (Cont x) = sqrt (getVariCont x)

getMode :: RandVar -> Vec Double
getMode (Disc x) = V.map fromIntegral (getModaDisc x)
getMode (Cont x) = getModaCont x


getModaDisc :: VarDisc -> Vec Int 
getModaDisc (Bin n p) | p == 0    = V.singleton 0
                      | p == 1    = V.singleton n
                      | isInt t   = V.fromList [m - 1, m]
                      | otherwise = V.singleton m
                     where
                         t = fromIntegral (n + 1) * p
                         m = floor t
getModaDisc (Poiss l) | l <= 0    = V.singleton 0
                      | isInt l   = V.fromList [m - 1, m]
                      | otherwise = V.singleton m
                       where
                         m = floor l
getModaDisc (Geo  _) = V.singleton 1
getModaDisc (Pasc r p) | r <= 1    = V.singleton 0
                       | p == 0    = V.singleton 0
                       | p == 1    = V.singleton 0
                       | otherwise = V.singleton (floor (fromIntegral (r - 1) * (1 - p) / p))

getModaDisc (Hiper m r n) = V.singleton (floor ((fromIntegral (n + 1) *
                                    fromIntegral (r + 1)) / fromIntegral (m + 2)))
getModaDisc (Custom xs ps) = let maxP = V.maximum ps
                                 eps = 1e-9
                              in V.map fst $
                                 V.filter (\(_, p) -> abs (p - maxP) < eps) $
                                 V.zip xs ps



getModaCont :: VarCont -> Vec Double
getModaCont (Norm  m _) = V.singleton m
getModaCont (Expo  _) = V.singleton 0
getModaCont (Unif _ _) = V.empty


getMaxP :: RandVar ->  Double
getMaxP (Disc x) = let v = getModaDisc x
                       k = fromIntegral (V.head v)
                       p = getProbDisc x Eq k
                    in p
getMaxP _ = error "Not definded"



-------------------------------------
-- Función de densidad de probabilidad y su punto máximo
-------------------------------------
------ FDP
getFDP :: RandVar -> Double ->  Double
getFDP (Cont (Norm m s)) x = (1 / (s * sqrt (2 * pi)) *
                              exp (- ((x - m)^2) / (2 * s^2)))
getFDP (Cont (Expo l)) x = if x < 0 then
                              0 else (l * exp (-l * x))
getFDP (Cont (Unif a b)) x = if x < a || x > b
                             then  0 else
                             (1 / (b - a))
getFDP (Disc _) _ = error "Not definded"
------ MaxFDP 

getMaxFDP :: RandVar ->  Double
getMaxFDP x@(Cont (Norm m _)) = getFDP x m
getMaxFDP x@(Cont (Expo _))   = getFDP x 0
getMaxFDP x@(Cont (Unif a _)) = getFDP x a
getMaxFDP (Disc _) = error "Not Definded"




getNames :: Markov -> Path
getNames (Mk n _) = n 

getMatrix :: Markov -> Matrix Double
getMatrix (Mk _ m) = m



getProbSteps :: Markov -> Name -> Name -> Int -> Double
getProbSteps (Mk names ma) n m k = let i = indexOf names n
                                       j = indexOf names m
                                       mat = matrixEx ma k
                                    in getElem mat i j 



getProbPath :: Markov -> Path -> Double
getProbPath (Mk names mat) c =
  let idxs = V.toList $ V.map (\x -> indexOf names x) c
  in product [ getElem mat i j | (i,j) <- zip idxs (tail idxs) ]