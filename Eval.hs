import ASL
import CBin
import Numeric.SpecFunctions (erf)


-------------------------------------
-- Evaluadores de operaciones binarias
-------------------------------------
evalOpBin :: OpBin -> NumC -> NumC -> NumC
evalOpBin Plus  (I n) (I m) = I (n + m)
evalOpBin Minus (I n) (I m) = I (n - m)
evalOpBin Times (I n) (I m) = I (n * m)
evalOpBin Div   (I n) (I m) = tryToInt (D (fromIntegral n / fromIntegral m))

evalOpBin Plus  (D n) (D m) = tryToInt (D (n + m))
evalOpBin Minus (D n) (D m) = tryToInt (D (n - m))
evalOpBin Times (D n) (D m) = tryToInt (D (n * m))
evalOpBin Div   (D n) (D m) = tryToInt (D (n / m))

evalOpBin op (D n) (I m) = evalOpBin op (D n) (toDouble (I m))
evalOpBin op (I n) (D m) = evalOpBin op (toDouble (I n)) (D m)


-----------------------------
-- Evaluadores de Probabilidad
-------------------------------
getProbDisc :: VarAle -> OpComp -> NumC -> NumC
getProbDisc (Disc x) op (I k) = D (getProbDisc x op k)
getProbCont (Cont x) op (D z) = D (getProbCont x op z)


getProbBt :: VarAle -> OpComp -> NumC -> OpComp -> NumC -> NumC
getProbBt (Disc x) op1 (I k1) op2 (I k2) = D (getProbBtDisc x op1 k1 op2 k2)
getProbBt (Cont x) op1 (D z1) op2 (D z2) = D (getProbBtDisc x op1 z1 op2 z2)

---- Discretas
getProbDisc :: VarDisc -> OpComp -> Int -> Double
getProbDisc var Eq k =
  case var of
    Bin (I n) (D p) -> fromIntegral (n |*| k) * p^k * (1 - p)^(n - k)
    Poiss (D l) -> exp (-l) * l^k / fromIntegral (fact k)
    Geo (D p) -> (1 - p)^(k - 1) * p
    Pasc (I r) (D p) -> fromIntegral ((k - 1) |*| (r - 1)) * p^r * (1 - p)^(k - r)
    Hiper (I m) (I r) (I n) -> fromIntegral ((r |*| k) * ((m - r) |*| (n - k))) /
                                              fromIntegral (m |*| n)
    _ -> error "getProbDisc: forma invalida"
getProbDisc var Lte k = sum [getProbDisc var Eq i | i <- [0 .. k]]
getProbDisc var Lt k = sum [getProbDisc var Eq i | i <- [0 .. k - 1]]
getProbDisc var Gt k  = 1 - getProbDisc var Lte k
getProbDisc var Gte k = 1 - getProbDisc var Lt k


getProbBtDisc :: VarDisc -> OpComp -> Int -> OpComp -> Int -> Double
getProbBtDisc var Gte k1 Lte k2 = sum [getProbDisc var Eq i | i <- [k1 .. k2]]
getProbBtDisc var Gt k1 Lte k2  = sum [getProbDisc var Eq i | i <- [k1 + 1 .. k2]]
getProbBtDisc var Gt k1 Lt k2   = sum [getProbDisc var Eq i | i <- [k1 + 1 .. k2 - 1]]
getProbBtDisc var Gte k1 Lt k2  = sum [getProbDisc var Eq i | i <- [k1 .. k2 - 1]]
getProbBtDisc _ _ _ _ _ = error "getProbBtDisc: combinacion de operadores no soportada"


--- Continuas
getProbCont :: VarCont -> OpComp -> Double -> Double
getProbCont _ Eq  _ = 0
getProbCont _ NEq _ = 1
getProbCont (Norm (D m) (D s)) Lte z = 0.5 * (1 + erf ((z - m) / (s * sqrt 2)))
getProbCont (Unif (D a) (D b)) Lte z | z < a     = 0
                                               | z > b     = 1
                                               | otherwise = (z - a) / (b - a)
getProbCont (Expo (D l)) Lte z | z < 0 = 0
                                    | otherwise = 1 - exp (- (l * z))
getProbCont var Lt z  = getProbCont var Lte z
getProbCont var Gt z  = 1 - getProbCont var Lte z
getProbCont var Gte z = 1 - getProbCont var Lt z
getProbCont _ _ _ = error "getProbCont: forma invalida"


getProbBtCont :: VarCont -> OpComp -> Double -> OpComp -> Double -> Double
getProbBtCont var Gt  z1 Lt  z2 = getProbCont var Lte z2 - getProbCont var Lte z1
getProbBtCont var Gte z1 Lt  z2 = getProbCont var Lte z2 - getProbCont var Lte z1
getProbBtCont var Gt  z1 Lte z2 = getProbCont var Lte z2 - getProbCont var Lte z1
getProbBtCont var Gte z1 Lte z2 = getProbCont var Lte z2 - getProbCont var Lt  z1

getProbBtCont _ _ _ _ _ =
  error "getProbBtCont: combinacion de operadores no soportada"


------------------------------
-- Evaluadores de funciones sobre variables aleatorias
-------------------------------
----- Esperanza
getEsp :: VarAle -> NumC
getEsp (Disc x) = D (getEspDisc x)
getEsp (Cont x) = D (getEspCont x)

getEspDisc :: VarDisc -> Double
getEspDisc (Bin (I n) (D p)) = fromIntegral n * p
getEspDisc (Poiss (D l)) = l
getEspDisc (Geo (D p)) = 1 / p
getEspDisc (Pasc (I r) (D p)) = fromIntegral r / p
getEspDisc (Hiper (I m) (I r) (I n)) = fromIntegral (n * r) / fromIntegral m
getEspDisc _ = error "getEspDisc: forma invalida"

getEspCont :: VarCont -> Double
getEspCont (Norm (D m) _) = m
getEspCont (Expo (D a)) = 1 / a
getEspCont (Unif (D a) (D b)) = (a + b) / 2
getEspCont _ = error "getEspCont: forma invalida"


--- Varianza
getVari :: VarAle -> NumC
getVari (Disc x) = D (getVariDisc x)
getVari (Cont x) = D (getVariCont x)

getVariDisc :: VarDisc -> Double
getVariDisc (Bin (I n) (D p)) = fromIntegral n * p * (1 - p)
getVariDisc (Poiss (D l)) = l
getVariDisc (Geo (D p)) = (1 - p) / p^2
getVariDisc (Pasc (I r) (D p)) = fromIntegral r * (1 - p) / p^2
getVariDisc (Hiper (I m) (I r) (I n)) = let m' = fromIntegral m
                                            r' = fromIntegral r
                                            n' = fromIntegral n
                                         in n' * (r'/m') * (1 - r'/m') *
                                           ((m' - n') / (m' - 1))
getVariDisc _ = error "getVariDisc: forma invalida"

getVariCont :: VarCont -> Double
getVariCont (Norm _ (D u)) = u^2
getVariCont (Expo (D a)) = 1 / a^2
getVariCont (Unif (D a) (D b)) = (b - a)^2 / 12
getVariCont _ = error "getVariCont: forma invalida"


--- Desvio Estandar
getDesv :: VarAle -> NumC
getDesv x = case getVari x of
              D n -> sqrt n
              _   -> error "getDev"

--Moda
getModa :: VarAle -> V.Vector NumC
getModa (Disc x) = ConstV (V.map I (getModaDisc x))
getModa (Cont x) = ConstV (V.map D (getModaCont x))

getModaDisc :: VarDisc -> V.Vector Int
getModaDisc (Bin (I n) (D p)) | p == 0    = V.singleton 0
                                        | p == 1    = V.singleton n
                                        | isInt t   = V.fromList [m - 1, m]
                                        | otherwise = V.singleton m
                                        where
                                          t = fromIntegral (n + 1) * p
                                          m = floor t
getModaDisc (Poiss (D l)) | l <= 0    = V.singleton 0
                               | isInt l   = V.fromList [m - 1, m]
                               | otherwise = V.singleton m
                               where
                                 m = floor l
getModaDisc (Geo (D _)) = V.singleton 0
getModaDisc (Pasc (I r) (D p)) | r == 1    = V.singleton 0
                                         | p == 0    = V.singleton 0
                                         | p == 1    = V.singleton 0
                                         | otherwise  = V.singleton (floor ((fromIntegral (r - 1)) *
                                                         (1 - p) / p))
getModaDisc (Hiper (I m) (I r) (I n)) = V.singleton (floor ((fromIntegral (n + 1)) *
                                                   (fromIntegral (r + 1)) / fromIntegral (m + 2)))

getModaCont :: VarCont -> V.Vector Double
getModaCont (Norm (D m) _) = V.singleton m
getModaCont (Expo (D _)) = V.singleton 0
getModaCont (Unif (D a) (D b)) = V.fromList [a, b]


-- Máxima probabilidad
getMaxP :: VarAle -> Double
getMaxP (Disc x) = let v = getModaDisc x
                       n = V.head v
                      in getProbDisc x Eq n
getMaxP _ = error "getMaxP solo aplica a variables discretas"



-- Función de densidad
getFDP :: VarCont -> Double -> Double
getFDP (Norm (D m) (D s)) x = 1 / (s * sqrt (2 * pi)) *
                                        exp (- (((z - m) ^ 2) / (2 * s ^ 2)) )
getFDP (Expo (D l)) x =  if z < 0 then 0
                              else l * exp (- (l * z))
getFDP (Unif (D a) (D b)) x = if z < a || z > b
                                        then 0 else 1 / (b - a)
getFDP _ _ = error "getFDP: distribución continua mal formada"


-- Máximo punto de la función de densidad
getMaxFDP :: VarCont -> Double
getMaxFDP x@(Norm (D m) _) = getFDP x m
getMaxFDP x@(Expo (D _)) = getFDP x 0
getMaxFDP x@(Unif (D a) (D b)) = getFDP x a

getMaxFDP _ = error "getMaxFDP: distribución continua mal formada"





evalExpNum :: (MonadState m, MonadError m) => ExpNum -> m NumV
evalExpNum (Const n) = return n
evalExpNum (VNum v) = do lookfor v
evalExpNum (UMinus e) = do e' <- evalExpNum
                           case e' of
                              D x -> return (D -x)
                              I x -> return (I -x)
evalExpNum (OpNum op n m) = do n' <- evalExpNum
                               m' <- evalExpNum
                               return (evalNumeric op' n'' m'')
evalExpNum (Access v n) = do v' <- evalExpVec
                             n' <- evalExpNum
                             access v' n'
evalExpNum (POp v op k) = do k' <- evalExpNum
                             v' <- evalExpAle
                             (v'', _, k'') <- checkProb
                             return (D (getProb v''' op k''))
evalExpNum (POpBt v op1 k1 op2 k2) = do k1' <- evalExpNum
                                        k2' <- evalExpNum
                                        v' <- evalExpAle
                                        (_, _,k1'', _ ,k2'') <- checkProbBt v'' op1 k1' op2 k2'
                                        return (D (getProbBt v'' op1, k1'', op2, k2''))

evalExpNum (Esp v) = do v'  <- evalExpAle
                        return (getEsp v')
evalExpNum (Vari v) = do v'  <- evalExpAle
                         return (getVari v')
evalExpNum (Desv v) = do v'  <- evalExpAle
                         return (getDesv v')
evalExpNum (FDP v x) = do v'  <- evalExpAle
                          x'  <- evalExpNum
                          (_, x'') <- checkFDP v' x'
                          return (D (getFDP v' x''))
evalExpNum (MaxP v) = do v'  <- evalExpAle
                         checkMaxP v'
                         return (D (getMaxP v'))
evalExpNum (MaxFDP v) = do v'  <- evalExpAle
                           chekMaxFDP v'
                           return (getMaxFDP v')





evalExpAle :: (MonadState m, MonadError m) => ExpAle -> m VarAle
evalExpAle (VAle v) = do  lookfor v
evalExpAle (Const x) = evalVarAle x


evalVarAle  ::(MonadState m, MonadError m) => VarAle -> m VarAle
evalVarAle (Disc x) = Disc <$> evalAleDisc x
evalVarAle (Cont x) = Cont <$> evalVarCont x

evalAleDisc :: (MonadState m, MonadError m) => VarDisc -> m VarDisc
evalAleDisc (Bin n p) = do n' <- evalExpNum n
                           p' <- evalExpNum p
                           checkVarDisc (Bin n' p')
evalAleDisc (Poiss l) = do l' <- evalExpNum l
                           checkVarDisc (Poiss l')
evalAleDisc (Geo p) = do p' <- evaleExpNum p
                         checkVarDisc (Geo p')
evalAleDisc (Hiper m r n) = do m' <- evalExpNum m
                               r' <- evalExpNum r
                               n' <- evalExpNum n
                               checkVarDisc (Hiper m' r' n')
evalAleDisc (Custom v1 v2) = do v1' <- evalExpVec
                                v2' <- evalExpVec
                                checkVarDisc (Custom v1' v2')



evalAleCont :: (MonadState m, MonadError m) => VarCont -> m VarCont
evalAleCont (Norm m u) = do m' <- evalExpNum m
                            u' <- evalExpNum u
                            checkVarCont (Norm m' u')
evalAleCont (Unif a b) = do a' <- evalExpNum a
                            b' <- evalExpNum b
                            checkVarCont (Unif a' b')
evalAleCont (Expo a) = do a' <- evalExpNum a
                          checkVarCont (Exp a')

