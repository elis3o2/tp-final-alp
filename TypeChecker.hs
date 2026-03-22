import ASL
import qualified Data.Vector as V


toDouble :: ExpNum -> ExpNum
toDouble (ConstI n) = (ConstD fromIntegral n)
toDouble (ConstD n) = (ConstD n)

isInt :: Double -> Bool
isInt x = x == fromIntegral (round x :: Int)
      && x >= fromIntegral (minBound :: Int)
      && x <= fromIntegral (maxBound :: Int)

tryToInt :: ExpNum -> ExpNum
tryToInt (ConstI n) = ConstI n
tryToInt (ConstD n) | isInt n   = ConstI (round n)
                    | otherwise = ConstD n


-----------------------------------
-- Checker de operaciones binarias
------------------------------------

checkOpBin :: (MonadState m, MonadError m) => OpBin -> ExpNum -> ExpNum -> m (OpBin, ExpNum, ExpNum)
checkOpBin Div _ (ConstD 0) = throw DivByZero
checkOpBin Div _ (ConstI 0) = throw DivByZero
checkOpBin op (ConstD n) (ConstD m) = return (op (ConstD n) (ConstD m))
checkOpBin op (ConstI n) (ConstI m) = return (op (ConstI n) (ConstI m))
checkOpBin op (ConstD n) (ConstI m) = return (op (ConstD n) (ConstI m))
checkOpBin op (ConstI n) (ConstD m) = return (op (ConstI n) (ConstD m))
checkOpBin _ _ _ = throw InvalidForm 


--------------------------------
-- Checker de probabilidades
--------------------------------
--- Un operador 
checkProb :: (MonadState m, MonadError m) => ExpAle -> OpComp -> ExpNum -> m (ExpAle -> OpComp -> ExpNum)
checkProb x@(Disc v) op k = checkProb' x op (tryToInt k) 
checkProb x@(Cont v) op z = checkProb' x op (toDouble k)

checkProb' :: (MonadState m, MonadError m) => ExpAle -> OpComp -> ExpNum -> m (ExpAle -> OpComp -> ExpNum)
checkProb' x@(Disc v) op n@(ConstI k) = do checkProbDisc v op k 
                                           return (v, op, n)
checkProb' (Disc v) op (ConstD k) = throw IntValuExpected 
checkProb' x@(Cont v) op n@(ConstD k1) = return (v op k1)
checkProb x op a = throw InvalidForm

checkProbDisc :: (MonadState m, MonadError m) => VarDisc -> OpComp -> Int -> m ()
checkProbDisc v@(Bin (ConstI n) _) op k = if k > n then throw InvalidForm else return ()
checkProbDisc v@(Pasc (ConstI r) _) op k = if k > r then throw InvalidForm else return ()
checkProbDisc v@(Hiper (ConstI m) _ _) op k = if k > m then throw InvalidForm else return ()
checkProbDisc var op k = if k < 0 then throw InvalidForm else return ()  


-- Dos operadores
checkProbBt :: (MonadState m, MonadError m) => 
    ExpAle -> OpComp -> ExpNum -> OpComp -> ExpNum 
    -> m (ExpAle, OpComp, ExpNum, OpComp, ExpNum)
checkProbBt x@(Disc v) op1 k1 op2 k2 = checkProbBt' x op1 (tryToInt k1) op (tryToInt k2)
checkProbBt x@(Cont v) op1 k1 op2 k2 = checkProbBt' x op1 (toDouble z1) op (toDouble z2)


checkProbBt' :: (MonadState m, MonadError m) => 
    ExpAle -> OpComp -> ExpNum -> OpComp -> ExpNum
    -> m (ExpAle, OpComp, ExpNum, OpComp, ExpNum)
checkProbBt' x@(Disc v) op1 a@(ConstI k1) op2 b@(ConstI k2) = do checkProbBt v op1 k1 op k2
                                                                 return x op1 a op2 b
checkProbBt' x@(Cont v) op1 a@(ConstD z2) op2 b@(ConstD z2) = do checkProbBt v op1 z1 op2 z2
                                                                 return x op1 a op2 b
checkProbBt' x op1 a op2 b = throw InvalidForm


checkProbBtDisc :: (MonadState s m, MonadError e m)=> VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
checkProbBtDisc v Gte k1 Lte k2 | k1 > k2  = throwError InvalidForm
                                | k1 < 0   = throwError InvalidForm
                                | otherwise = checkUpper v Gte k1 Lte k2
checkProbBtDisc v Gt k1 Lte k2 | k1 + 1 > k2 = throwError InvalidForm
                               | k1 + 1 < 0  = throwError InvalidForm
                               | otherwise   = checkUpper v Gt k1 Lte k2
checkProbBtDisc v Gt k1 Lt k2 | k1 + 1 > k2 - 1 = throwError InvalidForm
                              | k1 + 1 < 0      = throwError InvalidForm
                              | otherwise       = checkUpper v Gt k1 Lt k2
checkProbBtDisc v Gte k1 Lt k2 | k1 > k2 - 1 = throwError InvalidForm
                               | k1 < 0      = throwError InvalidForm
                               | otherwise   = checkUpper v Gte k1 Lt k2



checkUpper :: (MonadError e m) => VarDisc -> OpComp -> Int -> OpComp -> Int -> m ()
checkUpper v@(Bin (ConstI n) _) op1 k1 op2 k2 | k2 > n   = throwError InvalidForm
                                              | otherwise = return ()
checkUpper v@(Pasc (ConstI r) _) op1 k1 op2 k2 | k2 < r   = throwError InvalidForm
                                               | otherwise = return ()
checkUpper v@(Hiper (ConstI m) _ _) op1 k1 op2 k2 | k2 > m   = throwError InvalidForm
                                                  | otherwise = return ()
checkUpper v op1 k1 op2 k2 = return ()



checkProbBtCont :: (MonadState s m, MonadError e m) => VarDisc -> OpComp -> Double -> OpComp -> Double -> m ()
checkProbBtCont v Gte k1 Lte k2 | k1 > k2  = throwError InvalidForm
                                | otherwise = checkUpper v Gte k1 Lte k2
checkProbBtCont v Gt k1 Lte k2 | k1 >= k2 = throwError InvalidForm
                               | otherwise   = checkUpper v Gt k1 Lte k2
checkProbBtCont v Gt k1 Lt k2 | k1 == k2  = throwError InvalidForm
                              | otherwise       = checkUpper v Gt k1 Lt k2
checkProbBtCont v Gte k1 Lt k2 | k1 >= k2 - 1 = throwError InvalidForm
                               | otherwise   = checkUpper v Gte k1 Lt k2

---------------------------
-- Checker de funciones
---------------------------

checkMaxFDP ::(MonadState m, MonadError m) =>  ExpAle  -> m (ExpAle)
checkMaxFDP vv@(Cont v) = return (vv)
checkMaxFDP _  = throw ContVarExpected 


checkFDP ::(MonadState m, MonadError m) =>  ExpAle -> ExpNum -> m (ExpAle, ExpNum)
checkFDP vv@(Cont v) xx@(ConstD x) = return (vv, xx)
checkFDP vv@(Cont v) xx@(ConstI x) = return (vv, toDouble(xx))
checkFDP (Disc v) _ = throw ContVarExpected
checkFDP _ _ = throw InvalidForm 

------------------------------------
-- Checker de variables aleatorias
------------------------------------
checkVarAle ::  (MonadState m, MonadError m) => ExpAle -> m ExpAle
checkVarAle (Disc x) = do x' <- checkVarDisc (convert x)
                          return (Disc x')
checkVarAle (Cont x) = do x' <- checkVarCont (convert x)
                          return (Cont x')


checkVarDisc :: (MonadState m, MonadError m) => VarDisc -> m VarDisc
checkVarDisc   (Bin (ConstD n) _) = throw IntValuExpected
checkVarDisc x@(Bin (ConstI n) (ConstD p)) | p < 0 || p > 1 = throw ProbBadForm
                                           | otherwise return x
checkVarDisc x@(Poiss (ConstD l)) | p < 0 || p > 1 = throw ProbBadForm
                                  | otherwise return x
checkVarDisc x@(Geo (ConstD p)) | p < 0 || p > 1 = throw ProbBadForm
                                | otherwise return x                        
checkVarDisc (Pasc (ConstD r) _) = throw IntValuExpected
checkVarDisc x@(Pasc (ConstI r) (ConstD p)) | p < 0 || p > 1 = throw ProbBadForm
                                            | otherwise return x
checkVarDisc (Hiper (ConstD m) _ _) = throw IntValuExpected
checkVarDisc (Hiper _ (ConstD n) _) = throw IntValuExpected
checkVarDisc (Hiper _ _ (ConstD r)) = throw IntValuExpected
checkVarDisc x@(Hiper (ConstI m) (ConstI n) (ConstI r)) | n > m = throw InvalidForm 
                                                        | r > m = throw InvalidForm
                                                        | otherwise return x 
checkVarDisc x = throw InvalidForm


checkVarCont :: (MonadState m, MonadError m) => VarCont -> m VarCont 
checkVarCont x@(Normal _ (Const u)) if  u < 0 then throw InvalidForm else return x
checkVarCont x@(Unif (ConstD a) (ConstD b)) = if a > b then InvalidForm else return x
checkVarCont x@(Expo (ConstD a)) if a < 0 then throw InvalidForm else return x 
chackVarCont x = throw InvalidForm




-- Conversor 
convertDisc :: VarDisc -> VarDisc
convertDisc (Bin n p) = Bin (tryToInt n) (toDouble p)
convertDisc (Poiss l) = Poiss (toDouble p)
convertDisc (Geo p) = Geo (toDouble p)
convertDisc (Pasc r p) = Bin (tryToInt r) (toDouble p)
convertDisc (Hiper m r n) = Hiper (tryToInt m) (tryToInt r) (tryToInt n)
convertDisc v = v


convertDisc :: VarCont -> VarCont
convert (Norm m u) = return (Norm (toDouble m) (toDouble u))
convert (Expo a) = return (Expo (toDouble a))
convert (Unif a b) = return (Unif (toDouble a) (toDouble b))
convert v = v


