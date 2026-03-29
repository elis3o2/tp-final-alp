module TypeChecker where
import Monad
import AST
import Common
import qualified Data.Vector as V
import qualified Data.Map as M
import Error
import Global




checkComm :: MonadProb m => Comm -> m ()
checkComm (Print x) = checkType x
checkComm (Let _ x) = checkType x

checkComm (Table x) = checkDiscExp x
checkComm (TableR x n m) = do checkDiscExp x
                              checkNumExp n
                              checkNumExp m
checkComm (Plot x) = checkAleExp x

------------------------------------
-- Checker de expresiones principal
-----------------------------------
checkType :: MonadProb m => Exp -> m ()
checkType (VarRef _) = return ()
checkType (NumE   x) = checkNumExp    x
checkType (VecE   x) = checkVecExp    x
checkType (RandE  x) = checkRand      x
checkType (StatE  x) = checkStatExp   x
checkType (ProbE  x) = checkProbExp   x
checkType (Markov x) = checkMarkovExp x 
checkType (NodeE  x) = checkNodeExp   x 
checkType (ChainE x) = return ()

--------------------------------------------------------------- +
-- Checker de probabilidades                                    |
--------------------------------------------------------------- |
checkProb :: MonadProb m => ProbExp -> m ()    
checkProb (Prob x _ n) = do checkRand x
                            checkNumExp  n                  -- |
checkProb (ProbBetween _ Eq _ _  _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ NEq _ _ _) = throwErrorE ProbInvalidForm  -- |   
checkProb (ProbBetween _ _ _ Eq  _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ _ _ NEq _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ Lt _ _  _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ Lte _ _ _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ _ _ Gt  _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween _ _ _ Gte _) = throwErrorE ProbInvalidForm  -- |
checkProb (ProbBetween  x _ n _  m) = do checkAleExp x
                                         checkNumExp n
                                         checkNumExp m



checkNum :: MonadProb m => NumExp -> m ()
checkNum (ConstN _) = return ()
checkNum (UMinus x) = checkNu   mExp x
checkNum (OpNum _ x y) = do checkNumExp x 
                            checkNumExp y


checkVec ::  MonadProb m => VecExp -> m ()
checkVec (ConstV v) = V.mapM_ checkNumExp v
checkVec _ = throwErrnorE invalid 


checkVecNum :: MonadProb m => VecExp -> m ()
checkVecNum (Access v n) = do checkVecExp v
                              checkNumExp n 

---------------------------------------
-- Checker de expresiones numericas
----------------------------------------
checkNumExp :: MonadProb m => Exp -> m ()
checkNumExp (VarRef  _) = return ()
checkNumExp (NumE    x) = checkNum x
checkNumExp (VecE    x) = checkVecNum x
checkNumExp (StatE   x) = checkStatNumExp x
checkNumExp (ProbE   x) = checkProbExp x
checkNumExp (MarkovE x) = checkMkNumExp x
checkNumExp (ProbE   x) = do checkProb x
checkNumExp _           = throwErrorE InvalidVarType




checkStatNumExp :: MonadProb m => StatExp -> m ()
checkStatNumExp (Mean     x) = checkRand x
checkStatNumExp (Variance x) = checkRand x
checkStatNumExp (StdDev   x) = checkRand x
checkStatNumExp (FDP x    n) = do checkContExp x
                                  checkNumExp n      
checkStatNumExp (MaxP     x) = checkDiscExp x
checkStatNumExp (MaxFDP   x) = checkContExp x
checkStatNumExp _ = trhowErorrE Int


checkStatVecExp :: MonadProb m => StatExp -> m ()
checkStatVecExp (Mode x) = checkRand x
checkStatNumExp _ = trhowErorrE Int

---------------------------------------
--Checker de expresiones vectoriales
---------------------------------------
checkVecExp :: MonadProb m => Exp -> m ()
checkVecExp (VarRef _) = return ()
checkVecExp (Mode   x) = checkRand x
checkVecExp (ConstV v) = V.mapM_ checkNumExp v
checkVecExp _          = throwErrorE InvalidVarType

----------------------------------------------
-- Checker de expresiones aleatorias
---------------------------------------------
checkRand :: MonadProb m => Exp -> m ()
checkRand (VarRef _)         = return ()
checkRand (Rand (DiscE x)) = checkDiscExp' x
checkRand (Rand (ContE x)) = checkContExp' x
checkRand _       = throwErrorE InvalidVarType


checkDiscExp :: MonadProb m => Exp -> m ()
checkDiscExp (VarRef _) = return ()
checkDiscExp (Rand (DiscE x)) = checkDiscExp' x
checkDiscExp _ = throwErrorE DiscVarExpected


checkContExp :: MonadProb m => Exp -> m ()
checkContExp (VarRef _)       = return ()
checkContExp (Rand (ContE x)) = checkContExp' x
checkContExp _ = throwErrorE ContVarExpected


checkDiscExp' :: MonadProb m => ExpDisc -> m ()
checkDiscExp' (BinE n p) = do checkNumExp n
                              checkNumExp p
checkDiscExp' (PoissE l) = checkNumExp l
checkDiscExp' (GeoE p) = checkNumExp p
checkDiscExp' (PascE r p) = do checkNumExp r
                               checkNumExp p
checkDiscExp' (HiperE m r n) = do checkNumExp m
                                  checkNumExp r
                                  checkNumExp n
checkDiscExp' (CustomE v s) = do checkVecExp v
                                 checkVecExp s


checkContExp' :: MonadProb m => ExpCont -> m ()
checkContExp' (NormE m u) = do checkNumExp m
                               checkNumExp u
checkContExp' (UnifE a b) = do checkNumExp a
                               checkNumExp b
checkContExp' (ExpoE a) = checkNumExp a



checkMarkov :: MonadProb m => MarkovExp -> m ()
checkMarkov (VarRef      _) = return ()
checkMarkov (MkMarkov    c) = return ()
checkMarkov (Stationary  c) = checkMarkovExp c
checkMarkov (NextDist  c n) = do checkMarkovExp c
                                    return ()
checkMkExp _ = throwErrorE InvalidProb


checkMkNumExp :: MonadProb m => MarkovExp -> m ()
checkMkNumExp (StepProb x _ _ n) = do checkMarkovExp x
                                      checkNumExp n
checkMkNumExp (PathProb x c) = do checkMarkovExp x
                                  checkChainExp c
checkMkNumExp (HitProb x _ _) = checkMarkovExp x


checkMKChainExp :: MonadProb m => MarkovExp -> m ()
checkMKChainExp (SimulFromName x _ n) = do checkMkExp x
                                           checkNumExp n
checkMKChainExp (SimulFromVec x v n) = do checkMarkovExp x
                                          checkVecExp v
                                          checkNumExp n 

checkChainExp :: MonadProb m => Exp -> m ()
checkChainExp (ConstCh _) = return ()
checkChainExp (SimulFromName x _ n) = do checkMarkovExp x
                                         checkNumExp n
checkChainExp (SimulFromVec x v n) = do checkMarkovExp x
                                        checkVecExp v
                                        checkNumExp n
    