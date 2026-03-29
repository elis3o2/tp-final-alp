module TypeChecker where
import Monad
import AST
import Common
import qualified Data.Vector as V
import qualified Data.Map as M
import Error
import Global





checkNumNum :: MonadProb m => NumExp -> m ()
checkNumNum (ConstN _) = return ()
checkNumNum (UMinus x) = checkNumExp x
checkNumNum (OpNum _ x y) = do checkNumExp x 
                               checkNumExp y

checkVecVec ::  MonadProb m => VecExp -> m ()
checkVecVec (ConstV v) = V.mapM_ checkNumExp v
checkVecVec _ = throwErrnorE invalid 

checkVecNum :: MonadProb m => VecExp -> m ()
checkVecNum (Access v n) = do checkVecExp v
                              checkNumExp n 


checkRand :: MonadProb m => RandExp -> m ()
checkRand (DiscE x) = checkDisc' x
checkRand (ContE x) = checkCont' x
checkRand _       = throwErrorE InvalidVarType


checkDisc :: MonadProb m => RandExp -> m ()
checkDisc (DiscE x) = checkDisc' x
checkDisc _ = throwErrorE DiscVarExpected

checkCont :: MonadProb m => RandExp -> m ()
checkCont (ContE x) = checkCont' x
checkCont _ = throwErrorE ContVarExpected


checkMarkovMk :: MonadProb m => MarkovExp -> m ()
checkMarkovMk (MkMarkov    c) = return ()
checkMarkovMk (Stationary  c) = checkMarkovExp c
checkMarkovMk (NextDist  c n) = do checkMarkovExp c
                                    return ()
checkMkExp _ = throwErrorE InvalidProb



checkMarkovNum :: MonadProb m => MarkovExp -> m ()
checkMarkovNum (StepProb x _ _ n) = do checkMarkovExp x
                                      checkNumExp n
checkMarkovNum (PathProb x c) = do checkMarkovExp x
                                   checkChainExp c
checkMarkovNum (HitProb x _ _) = checkMarkovExp x


checkMarKovChain :: MonadProb m => MarkovExp -> m ()
checkMarKovChain (SimulFromName x _ n) = do checkMarkovExp x
                                           checkNumExp n
checkMarKovChain (SimulFromVec x v n) = do checkMarkovExp x
                                           checkVecExp v
                                           checkNumExp n 





checkStatNum :: MonadProb m => StatExp -> m ()
checkStatNum (Mean     x) = checkRand x
checkStatNum (Variance x) = checkRand x
checkStatNum (StdDev   x) = checkRand x
checkStatNum (FDP x    n) = do checkContExp x
                               checkNumExp n      
checkStatNum (MaxP     x) = checkDiscExp x
checkStatNum (MaxFDP   x) = checkContExp x
checkStatNum _ = trhowErorrE Int


checkStatVec :: MonadProb m => StatExp -> m ()
checkStatVec (Mode x) = checkRand x
checkStatNum _ = trhowErorrE Int



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
checkProb (ProbBetween  x _ n _  m) = do checkRand   x
                                         checkNumExp n
                                         checkNumExp m



checkNumExp :: MonadProb m => Exp -> m ()
checkNumExp (VarRef  _) = return ()
checkNumExp (NumE    x) = checkNumNum x
checkNumExp (VecE    x) = checkVecNum x
checkNumExp (StatE   x) = checkStatNum x
checkNumExp (ProbE   x) = checkProb x
checkNumExp (MarkovE x) = checkMarkovNum x
checkNumExp _           = throwErrorE InvalidVarType


checkVecExp :: MonadProb m => Exp -> m ()
checkVecExp (VarRef  _) = return ()
checkVecExp (VecE    x) = checkVecVec x
checkNumExp _           = throwErrorE InvalidVarType

checkRandExp :: MonadProb m => Exp -> m ()
checkRandExp (VarRef  _) = return ()
checkRandExp (RandE x)   = checkRand x
checkRandExp _ = throwErrorE error 


checkDiscExp :: MonadProb m => Exp -> m ()
checkDiscExp (VarRef  _) = return ()
checkDiscExp (RandE x)   = checkDisc x
checkDiscExp _ = throwErrorE DiscVarExpected

checkContExp :: MonadProb m => Exp -> m ()
checkContExp (VarRef  _) = return ()
checkContExp (RandE x)   = checkCont x
checkContExp _ = throwErrorE ContVarExpected



checkMarkovExp :: MonadProb m => Exp -> m ()
checkMarkovExp (VarRef  _) = return () 
checkMarkovExp (MarkovE x) = checkMarkovMk x 
checkMarkovExp _           = throwErrorE InvalidVarType


checkChainExp :: MonadProb m => Exp -> m ()
checkChainExp (VarRef  _) = return ()
checkChainExp ()

checkDisc' :: MonadProb m => ExpDisc -> m ()
checkDisc' (BinE n p) = do checkNumExp n
                           checkNumExp p
checkDisc' (PoissE l) = checkNumExp l
checkDisc' (GeoE p) = checkNumExp p
checkDisc' (PascE r p) = do checkNumExp r
                            checkNumExp p
checkDisc' (HiperE m r n) = do checkNumExp m
                               checkNumExp r
                               checkNumExp n
checkDisc' (CustomE v s) = do checkVecExp v
                              checkVecExp s
checkCont' :: MonadProb m => ExpCont -> m ()
checkCont' (NormE m u) = do checkNumExp m
                               checkNumExp u
checkCont' (UnifE a b) = do checkNumExp a
                               checkNumExp b
checkCont' (ExpoE a) = checkNumExp a

