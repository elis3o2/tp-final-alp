import ASL
import CBin
import qualified Data.Vector as V
import Numeric.SpecFunctions (erf)

-------------------------------------
-- Helpers
-------------------------------------

toDouble :: NumC -> NumC
toDouble (I n) = D (fromIntegral n)
toDouble d     = d

tryToInt :: NumC -> NumC
tryToInt (D x) | isInt x  = I (round x)
               | otherwise = D x
tryToInt x = x

isInt :: Double -> Bool
isInt x = fromIntegral (round x) == x

-------------------------------------
-- Evaluadores de operaciones binarias
-------------------------------------

evalOpBin :: (MonadError e m) => OpBin -> NumC -> NumC -> m NumC
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

getProb :: VarAle -> OpComp -> NumC -> NumC
getProb (Disc x) op (I k) = D (getProbDisc x op k)
getProb (Cont x) op (D z) = D (getProbCont x op z)
getProb _ _ _ = error "getProb: tipo incorrecto"

getProbBt :: VarAle -> OpComp -> NumC -> OpComp -> NumC -> NumC
getProbBt (Disc x) op1 (I k1) op2 (I k2) =
  D (getProbBtDisc x op1 k1 op2 k2)
getProbBt (Cont x) op1 (D z1) op2 (D z2) =
  D (getProbBtCont x op1 z1 op2 z2)
getProbBt _ _ _ _ _ = error "getProbBt: tipo incorrecto"


------ Discretas

getProbDisc :: VarDisc -> OpComp -> Int -> Double
getProbDisc var Eq k =
  case var of
    Bin (Const (I n)) (Const (D p)) -> fromIntegral (n |*| k) * p^k *
                                       (1 - p)^(n - k)

    Poiss (Const (D l)) -> exp (-l) * l^k / fromIntegral (fact k)

    Geo (Const (D p)) -> (1 - p)^(k - 1) * p

    Pasc (Const (I r)) (Const (D p)) -> fromIntegral ((k - 1) |*| (r - 1)) *
                                        p^r * (1 - p)^(k - r)

    Hiper (Const (I m)) (Const (I r)) (Const (I n)) -> fromIntegral ((r |*| k) * ((m - r) |*| (n - k)))
                                                      / fromIntegral (m |*| n)
    _ -> error "getProbDisc: forma invalida"

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

getProbCont (Norm (Const (D m)) (Const (D s))) Lte z = 0.5 * (1 + erf ((z - m) /
                                                              (s * sqrt 2)))

getProbCont (Unif (Const (D a)) (Const (D b))) Lte z | z < a     = 0
                                                     | z > b     = 1
                                                     | otherwise = (z - a) / (b - a)
getProbCont (Expo (Const (D l))) Lte z | z < 0     = 0
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
getEsp :: VarAle -> NumC
getEsp (Disc x) = D (getEspDisc x)
getEsp (Cont x) = D (getEspCont x)

getEspDisc :: VarDisc -> Double
getEspDisc (Bin (Const (I n)) (Const (D p))) = fromIntegral n * p
getEspDisc (Poiss (Const (D l))) = l
getEspDisc (Geo (Const (D p))) = 1 / p
getEspDisc (Pasc (Const (I r)) (Const (D p))) = fromIntegral r / p
getEspDisc _ = error "getEspDisc"

getEspCont :: VarCont -> Double
getEspCont (Norm (Const (D m)) _) = m
getEspCont (Expo (Const (D a))) = 1 / a
getEspCont (Unif (Const (D a)) (Const (D b))) = (a + b) / 2
getEspCont _ = error "getEspCont"


----- Varianza
getVari :: VarAle -> NumC
getVari (Disc x) = D (getVariDisc x)
getVari (Cont x) = D (getVariCont x)

getVariDisc :: VarDisc -> Double
getVariDisc (Bin (Const (I n)) (Const (D p))) = fromIntegral n * p * (1 - p)
getVariDisc (Poiss (Const (D l))) = l
getVariDisc _ = error "getVariDisc"

getVariCont :: VarCont -> Double
getVariCont (Norm _ (Const (D s))) = s^2
getVariCont (Expo (Const (D a))) = 1 / a^2
getVariCont (Unif (Const (D a)) (Const (D b))) = (b - a)^2 / 12
getVariCont _ = error "getVariCont"


---- Desvio Estandar
getDesv :: VarAle -> NumC
getDesv v = case getVari v of
  D x -> D (sqrt x)
  _   -> error "getDesv"


---- Moda
getModa :: VarAle -> V.Vector NumC
getModa (Disc x) = V.map I (getModaDisc x)
getModa (Cont x) = V.map D (getModaCont x)

getModaDisc :: VarDisc -> V.Vector Int
getModaDisc (Bin (Const (I n)) (Const (D p))) | p == 0    = V.singleton 0
                                              | p == 1    = V.singleton n
                                              | isInt t   = V.fromList [m - 1, m]
                                              | otherwise = V.singleton m
                                              where
                                                t = fromIntegral (n + 1) * p
                                                m = floor t
getModaDisc (Poiss (Const (D l))) | l <= 0    = V.singleton 0
                                  | isInt l   = V.fromList [m - 1, m]
                                  | otherwise = V.singleton m
                                  where
                                    m = floor l
getModaDisc (Geo (Const (D _))) = V.singleton 1   -- importante: geométrica arranca en 1
getModaDisc (Pasc (Const (I r)) (Const (D p))) | r <= 1    = V.singleton 0
                                                | p == 0    = V.singleton 0
                                                | p == 1    = V.singleton 0
                                                | otherwise =
                                                    V.singleton (floor (fromIntegral (r - 1) * (1 - p) / p))

getModaDisc (Hiper (Const (I m)) (Const (I r)) (Const (I n))) = V.singleton (floor ((fromIntegral (n + 1) * 
                                                              fromIntegral (r + 1)) / fromIntegral (m + 2)))
getModaDisc _ = error "getModaDisc: forma invalida"


getModaCont :: VarCont -> V.Vector Double
getModaCont (Norm (Const (D m)) _) = V.singleton m
getModaCont (Expo (Const (D _))) = V.singleton 0
getModaCont _ = error "getModaCont: forma invalida"


------------------------------------
-- Punto de máxima probabilidad
------------------------------------
getMaxP :: VarAle -> NumC
getMaxP (Disc x) = let v = getModaDisc x
                       k = V.head v
                    in D (getProbDisc x Eq k)
getMaxP _ = error "getMaxP: solo para discretas"


-------------------------------------
-- Función de densidad de probabilidad y su punto máximo
-------------------------------------
------ FDP
getFDP :: VarCont -> Double -> NumC
getFDP (Norm (Const (D m)) (Const (D s))) x = D (1 / (s * sqrt (2 * pi)) *
                                        exp ( - ((x - m)^2) / (2 * s^2) ))
getFDP (Expo (Const (D l))) x = if x < 0 then D 0
                                else D (l * exp (-l * x))
getFDP (Unif (Const (D a)) (Const (D b))) x = if x < a || x > b
                                              then D 0 else D (1 / (b - a))
getFDP _ _ = error "getFDP: forma invalida"


------ MaxFDP 
getMaxFDP :: VarCont -> NumC
getMaxFDP x@(Norm (Const (D m)) _) = getFDP x m
getMaxFDP x@(Expo (Const (D _)))   = getFDP x 0
getMaxFDP x@(Unif (Const (D a)) _) = getFDP x a
getMaxFDP _ = error "getMaxFDP"


-------------------------------------
-- Evaluación de expresiones
-------------------------------------

evalExpNum :: (MonadState s m, MonadError e m) => ExpNum -> m NumC

evalExpNum (Const n) = return n
evalExpNum (VNum v) = lookfor v
evalExpNum (UMinus e) = do v <- evalExpNum e
                           case v of
                              I x -> return (I (-x))
                              D x -> return (D (-x))
evalExpNum (OpNum op a b) = do  a' <- evalExpNum a
                                b' <- evalExpNum b
                                evalOpBin op a' b'
evalExpNum (POp v op k) = do v' <- evalExpAle v
                             k' <- evalExpNum k
                             return (getProb v' op k')
evalExpNum (POpBt v op1 k1 op2 k2) = do v'  <- evalExpAle v
                                        k1' <- evalExpNum k1
                                        k2' <- evalExpNum k2
                                        return (getProbBt v' op1 k1' op2 k2')
evalExpNum (Esp v) = getEsp <$> evalExpAle v
evalExpNum (Vari v) = getVari <$> evalExpAle v
evalExpNum (Desv v) = getDesv <$> evalExpAle v
evalExpNum (MaxP v) = do v' <- evalExpAle v
                         checkMaxP v'
                         return (getMaxP v')
evalExpNum (FDP v x) = do v' <- evalExpAle v
                          x' <- evalExpNum x
                          (_, x'') <- checkFDP v' x'
                          return (getFDP v' x'')
evalExpNum (MaxFDP v) = do v' <- evalExpAle v
                           checkMaxFDP v'
                           return (getFDP v')


-----------------------------------
---
------------------------------------






-------------------------------------
-- Variables aleatorias
-------------------------------------

evalExpAle :: (MonadState s m, MonadError e m) => ExpAle -> m VarAle
evalExpAle (VAle v) = lookfor v
evalExpAle (Const v) = evalVarAle v

evalVarAle :: (MonadState s m, MonadError e m) => VarAle -> m VarAle
evalVarAle (Disc x) = Disc <$> evalAleDisc x
evalVarAle (Cont x) = Cont <$> evalAleCont x

evalAleDisc :: (MonadState s m, MonadError e m) => VarDisc -> m VarDisc
evalAleDisc (Bin n p) = Bin <$> evalExpNum n <*> evalExpNum p
evalAleDisc (Poiss l) = Poiss <$> evalExpNum l
evalAleDisc (Geo p) = Geo <$> evalExpNum p
evalAleDisc (Pasc r p) = Pasc <$> evalExpNum r <*> evalExpNum p
evalAleDisc (Hiper m r n) = Hiper <$> evalExpNum m <*> evalExpNum r <*> evalExpNum n
evalAleDisc x = return x

evalAleCont :: (MonadState s m, MonadError e m) => VarCont -> m VarCont
evalAleCont (Norm m s) = Norm <$> evalExpNum m <*> evalExpNum s
evalAleCont (Unif a b) = Unif <$> evalExpNum a <*> evalExpNum b
evalAleCont (Expo a) = Expo <$> evalExpNum a