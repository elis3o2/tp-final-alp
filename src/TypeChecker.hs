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
checkType (VarRef _)        = return ()
checkType x@(ConstN  {}) = checkNumExp x
checkType x@(UMinus  {}) = checkNumExp x
checkType x@(OpNum   {}) = checkNumExp x
checkType x@(Access  {}) = checkNumExp x
checkType x@(Mean     {}) = checkNumExp x
checkType x@(Variance    {}) = checkNumExp x
checkType x@(StdDev    {}) = checkNumExp x
checkType x@(FDP     {}) = checkNumExp x
checkType x@(MaxP    {}) = checkNumExp x
checkType x@(MaxFDP  {}) = checkNumExp x
checkType x@(Prob     {}) = checkNumExp x
checkType x@(ProbBetween   {}) = checkProb   x
checkType x@(Mode    {}) = checkVecExp x
checkType x@(ConstV  {}) = checkVecExp x
checkType x@(Rand     {}) = checkAleExp x
checkType x@(Node    {}) = checkMkExp  x 
checkType x@(MkE   {}) = checkMkExp  x
--------------------------------------------------------------- +
-- Checker de probabilidades                                    |
--------------------------------------------------------------- |
checkProb :: MonadProb m => Exp -> m ()                      -- |
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
checkProb _ = throwErrorE InvalidVarType

---------------------------------------
-- Checker de expresiones numericas
----------------------------------------
checkNumExp :: MonadProb m => Exp -> m ()
checkNumExp (VarRef _)    = return ()
checkNumExp (ConstN _) = return ()
checkNumExp (UMinus x) = checkNumExp x
checkNumExp (OpNum  _ x y) = do checkNumExp x
                                checkNumExp y
checkNumExp (Access v n) = do checkVecExp v
                              checkNumExp n
checkNumExp (Mean      x) = checkAleExp x
checkNumExp (Variance     x) = checkAleExp x
checkNumExp (StdDev     x) = checkAleExp x
checkNumExp (FDP    x n) = do checkContExp x
                              checkNumExp n
checkNumExp (MaxP     x) = checkDiscExp x
checkNumExp (MaxFDP   x) = checkContExp x
checkNumExp (Prob  x _ n) = do checkAleExp x
                              checkNumExp n
checkNumExp x@(ProbBetween {}) = checkProb x
checkNumExp _            = throwErrorE InvalidVarType



---------------------------------------
--Checker de expresiones vectoriales
---------------------------------------
checkVecExp :: MonadProb m => Exp -> m ()
checkVecExp (VarRef    _) = return ()
checkVecExp (Mode   x) = checkAleExp x
checkVecExp (ConstV v) = V.mapM_ checkNumExp v
checkVecExp _          = throwErrorE InvalidVarType


----------------------------------------------
-- Checker de expresiones aleatorias
---------------------------------------------
checkAleExp :: MonadProb m => Exp -> m ()
checkAleExp (VarRef _)         = return ()
checkAleExp (Rand (DiscE x)) = checkDiscExp' x
checkAleExp (Rand (ContE x)) = checkContExp' x
checkAleExp _       = throwErrorE InvalidVarType


checkDiscExp :: MonadProb m => Exp -> m ()
checkDiscExp (VarRef _) = return ()
checkDiscExp (Rand (DiscE x)) = checkDiscExp' x
checkDiscExp _ = throwErrorE DiscVarExpected


checkContExp :: MonadProb m => Exp -> m ()
checkContExp (VarRef _) = return ()
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




checkMkExp :: MonadProb m => Exp -> m ()
checkMkExp (Node []) = throwErrorE AleInvalidForm
checkMkExp (Node xs) = do checkng xs
                            where
                                checkng [] = return ()
                                checkng ((_,y):ys) = do checkNumExp y
                                                        checkng ys
checkMkExp (MkE _) = return ()
checkMkExp _ = throwErrorE InvalidProb