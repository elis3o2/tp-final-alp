module Eval where
import AST 
import TypeChecker
import Monad
import Error
-------------------------
--- Helpers de NumC
-------------------------

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x :: Int)
      && x >= fromIntegral (minBound :: Int)
      && x <= fromIntegral (maxBound :: Int)


toInt :: MonadProb m => NumC -> m Int
toInt (I n) = n
toInt (D n) | isInt n   = return (I (round n))
            | otherwise = throwError IntValueExpected

toDouble :: MonadProb => NumC -> m Double
toDouble (I n) = return (fromIntegral n)
toDouble (D n) = return n


-------------------------------------
-- Evaluadores de operaciones binarias
-------------------------------------

evalOpBin :: MonadProb m => OpBin -> NumC -> NumC -> m NumC
evalOpBin Div _ (I 0) = throwError DivByZero
evalOpBin Div _ (D 0) = throwError DivByZero

evalOpBin Plus  (I n) (I m) = return (I (n + m))
evalOpBin Minus (I n) (I m) = return (I (n - m))
evalOpBin Times (I n) (I m) = return (I (n * m))
evalOpBin Div   (I n) (I m) = return $ tryToInt (D (fromIntegral n / fromIntegral m))

evalOpBin Plus  (D n) (D m) = return (tryToInt (D (n + m)))
evalOpBin Minus (D n) (D m) = return (tryToInt (D (n - m)))
evalOpBin Times (D n) (D m) = return (tryToInt (D (n * m)))
evalOpBin Div   (D n) (D m) = return (tryToInt (D (n / m)))

evalOpBin op x y = evalOpBin op (toDouble x) (toDouble y)


-------------------------------------
-- Probabilidad
-------------------------------------

getProb :: MonadProb m => VarAle -> OpComp -> NumC -> m Double
getProb (Disc x) op k = do k' <- toInt k
                           checkProbDisc x op k'
                           return (getProbDisc x op k')
                      
getProb (Cont x) op z = do z' <- toDouble z
                           return (getProbCont x op z')



getProbBt :: MonadProb m => VarAle -> OpComp -> NumC -> OpComp -> NumC -> m Double
getProbBt (Disc x) op1 k1 op2 k2 = do k1' <- toInt k1
                                      k2' <- toInt k2
                                      checkProbBtDisc x op1 k1' op2 k2'
                                      return (getProbBtDisc x op1 k1' op2 k2')

getProbBt (Cont x) op1 z1 op2 z2 = do z1' <- toDouble z1
                                      z2' <- toDouble z2
                                      checkProbBtCont x op1 z1' op2 z2'
                                      return (getProbBtCont x op1 z1' op2 z2')

------ Discretas
getProbDisc :: VarDisc -> OpComp -> Int -> Double

getProbDisc (Bin n p) Eq k  = fromIntegral (n |*| k) * p^k *
                            (1 - p)^(n - k)
    
getProbDisc (Poiss l) Eq k = exp (-l) * l^k / fromIntegral (fact k)

getProbDisc (Geo p) Eq k = (1 - p)^(k - 1) * p

getProbDisc (Pasc r p) Eq k = fromIntegral ((k - 1) |*| (r - 1)) *
                                        p^r * (1 - p)^(k - r)

getProbDisc (Hiper m r n) Eq k = fromIntegral ((r |*| k) * ((m - r) |*| (n - k)))
                               / fromIntegral (m |*| n)

getProbDisc (Custom v1 v2) Eq k = v2 V.! (V.look k v1)

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
getProbCont _ _ _ = error "getProbCont"

getProbBtCont :: VarCont -> OpComp -> Double -> OpComp -> Double -> Double
getProbBtCont var _ z1 _ z2 = getProbCont var Lte z2 - getProbCont var Lte z1


----------------------------------------
-- Esperanza / Varianza / Desvío / Moda
----------------------------------------
----- Esperanza
getEsp ::  VarAle -> Double
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
getVari :: VarAle -> Double
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
getDesv :: VarAle -> Double
getDesv (Disc x) = sqrt (getVariDisc x)
getDesv (Cont x) = sqrt (getVariCont x)


---- Moda
getModa :: MonadProb m => VarAle -> m Vec NumC
getModa (Disc (Unif _ _)) = throwError NotDefined
getModa (Disc x) = return $ V.map I (getModaDisc x)
getModa (Cont x) = return $ V.map D (getModaCont x)

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
getModaCont (Norm  m u) = V.singleton m
getModaCont (Expo  _) = V.singleton 0
getModaCont _ = error "getModaCont"



------------------------------------
-- Máxima probabilidad
------------------------------------
getMaxP :: MonadProb m => VarAle -> m Double
getMaxP (Disc x) = let v = getModaDisc x
                       k = V.head v
                       p = getProbDisc x Eq k
                    in return p
getMaxP (Cont x) = error throwError DiscVarExpected


-------------------------------------
-- Función de densidad de probabilidad y su punto máximo
-------------------------------------
------ FDP
getFDP :: MonadProb m => VarAle -> Double -> m Double
getFDP (Cont (Norm m s)) x = return (1 / (s * sqrt (2 * pi)) *
                                   exp (- ((x - m)^2) / (2 * s^2)))
getFDP (Cont (Expo l)) x = if x < 0 then 
                              return 0 else return (l * exp (-l * x))
getFDP (Cont (Unif a b)) x = if x < a || x > b 
                             then return 0 else
                             return (1 / (b - a))
getFDP (Disc _) _ = throwError DiscVarExpected
------ MaxFDP 

getMaxFDP :: MonadProb m => VarAle -> m Double
getMaxFDP x@(Cont (Norm m _)) = getFDP x m
getMaxFDP x@(Cont (Expo _))   = getFDP x 0
getMaxFDP x@(Cont (Unif a _)) = getFDP x a
getMaxFDP (Disc _) = throwError ContVarExpected
-------------------------------------
-- Funciones sobre vectores 
-------------------------------------
access :: MonadProb m => ExpVec -> Int -> m NumC 
access v i | i < 0 || i > V.length v - 1 = throwError InvalidIndex
           | otherwise = return (v V.! i)


-------------------------------------
-- Evaluación de expresiones Numericas
-------------------------------------

evalExpNum :: MonadProb m => ExpNum -> m NumC
evalExpNum (Const n) = return n
evalExpNum (VNum v)   = lookupNum v

evalExpNum (POp v op k) = do v' <- evalExpAle v
                             k' <- evalExpNum k
                             p  <- getProb v' op k'
                             return (D p)

evalExpNum (POpBt v op1 k1 op2 k2) = do v' <- evalExpAle v
                                        k1' <- evalExpNum k1
                                        k2' <- evalExpNum k2
                                        p   <- getProbBt v' op1 k1' op2 k2'
                                        return (D p)

evalExpNum (Access v n) = do n' <- evalExpNum n >>= toInt
                             v' <- evalExpVec v
                             return (access v' n')

evalExpNum (Esp v) = do v' <- evalExpAle v
                        return (D (getEsp v'))

evalExpNum (Vari v) = do v' <- evalExpAle v
                         return (D (getVari v'))

evalExpNum (Desv v) = do v' <- evalExpAle v
                         return (D (getDesv v'))

evalExpNum (MaxP v) = do v' <- evalExpAle v
                         k  <- getMaxP v'
                         return (D k)

evalExpNum (FDP v x) = do v' <- evalExpAle v
                          x' <- evalExpNum x >>= toDouble
                          d  <- getFDP v' (D x')
                          return (D d)

evalExpNum (MaxFDP v) = do v' <- evalExpAle v
                           d  <- getMaxFDP v'
                           return (D d)

-------------------------------------
-- Evaluador de Variables aleatorias
-------------------------------------

evalExpAle :: MonadProb m => ExpAle -> m VarAle
evalExpAle (VarA v)  = lookupAle v
evalExpAle (DiscE x) = Disc <$> evalExpDisc x
evalExpAle (ContE x) = Cont <$> evalExpCont x


evalExpDisc :: MonadProb m => VarDiscExp -> m VarDisc
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

evalExpDisc (CustomE v p) = do v' <- evalExpVec v
                               p' <- evalExpVec p
                               return (Custom v' p')

evalExpCont :: MonadProb m => VarContExp -> m VarCont
evalExpCont (NormE m s) = do m' <- evalExpNum m >>= toDouble
                             s' <- evalExpNum s >>= toDouble
                             return (Norm m' s')

evalExpCont (UnifE a b) = do a' <- evalExpNum a >>= toDouble
                             b' <- evalExpNum b >>= toDouble
                             return (Unif a' b')

evalExpCont (ExpoE a) = do a' <- evalExpNum a >>= toDouble
                            return (Expo a')

--------------------------------
--- Evaluador de Vectores
--------------------------------

evalExpVec :: MonadProb m => ExpVec -> m (Vec NumC)
evalExpVec (VarV x)    = lookupVec x
evalExpVec (ConstV v)  = V.mapM evalExpNum v
evalExpVec (Moda x) = do x' <- evalExpAle x
                         return (getModa x')