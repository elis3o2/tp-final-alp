{-|
Module      : TypeChecker
Description : Static type checker for expressions and commands.
              Variables are registered in the global state with a Dummy value
              during type checking to enable forward error detection.
-}
module TypeChecker where
import Monad
import AST
import Common
import qualified Data.Vector as V
import qualified Data.Map as M
import Error
import Control.Monad.Except (MonadError(catchError))

-- =====================================================
-- | Command Checker                                 
-- =====================================================
checkComm :: MonadProb m => Comm -> m ()
checkComm (Print x) = do _ <- checkType x
                         return ()
checkComm (Let n x) = do t <- checkType x
                         addDummyDecl n t
checkComm (Table x) = checkDiscExp x
checkComm (TableR x n m) = do checkDiscExp x
                              checkNumExp n
                              checkNumExp m
checkComm (Plot x) = do catchError (checkRandExp x) (\_ -> checkMarkovExp x)
checkComm (LetN n x) = do checkNodeExp x
                          addNodeDummy n
 
-- =======================================================
-- | Main Expression Checker                         
-- =======================================================
checkType :: MonadProb m => Exp -> m Type
checkType   (VarRef n)        = do d <- getDecls
                                   case M.lookup n d of
                                      Just (_,ty) -> return ty
                                      Nothing     ->
                                        throwErrorT VarNotScope
checkType x@(ConstN       {}) = do checkNumExp x
                                   return Num
checkType x@(UMinus       {}) = do checkNumExp x
                                   return Num
checkType x@(OpNum        {}) = do checkNumExp x
                                   return Num
checkType x@(Access       {}) = do checkNumExp x
                                   return Num
checkType x@(Mean         {}) = do checkNumExp x
                                   return Num
checkType x@(Variance     {}) = do checkNumExp x
                                   return Num
checkType x@(StdDev       {}) = do checkNumExp x
                                   return Num
checkType x@(PDF          {}) = do checkNumExp x
                                   return Num
checkType x@(MaxP         {}) = do checkNumExp x
                                   return Num
checkType x@(MaxPDF       {}) = do checkNumExp x
                                   return Num
checkType x@(Prob         {}) = do checkNumExp x
                                   return Num
checkType x@(ProbBetween  {}) = do checkProb   x
                                   return Num
checkType x@(Mode         {}) = do checkVecExp x
                                   return Vec
checkType x@(ConstV       {}) = do checkVecExp x
                                   return Vec
checkType x@(Rand  (DiscE _)) = do checkRandExp x
                                   return RandDisc
checkType x@(Rand  (ContE _)) = do checkRandExp x
                                   return RandCont
checkType x@(Markov       {}) = do checkMarkovExp x
                                   return Mark
checkType x@(ConstCh      {}) = do checkPathExp x
                                   return Path
checkType x@(ProbStep     {}) = do checkNumExp x
                                   return Num
checkType x@(ProbPath     {}) = do checkNumExp x
                                   return Num
checkType x@(ProbHit      {}) = do checkNumExp x
                                   return Num
checkType x@(NextDist     {}) = do checkMarkovExp x
                                   return Mark
checkType x@(Stationary   {}) = do checkVecExp x
                                   return Vec
checkType x@(SimulFromName {}) = do checkPathExp x
                                    return Path
checkType x@(SimulFromVec  {}) = do checkPathExp x
                                    return Path

-- =========================================================
-- | Probability Checker
-- =========================================================
checkProb :: MonadProb m => Exp -> m ()
checkProb (ProbBetween _ Eq _ _  _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ NEq _ _ _) = throwErrorE ProbInvalidForm   
checkProb (ProbBetween _ _ _ Eq  _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ _ _ NEq _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ Lt _ _  _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ Lte _ _ _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ _ _ Gt  _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween _ _ _ Gte _) = throwErrorE ProbInvalidForm
checkProb (ProbBetween  x _ n _  m) = do checkRandExp x
                                         checkNumExp n
                                         checkNumExp m
checkProb _ = throwErrorT InvalidVarType


-- ========================================================
-- | Numeric Expression Checker
-- ========================================================
checkNumExp :: MonadProb m => Exp -> m ()
checkNumExp (VarRef n)    = lookTy n Num
checkNumExp (ConstN _) = return ()
checkNumExp (UMinus x) = checkNumExp x
checkNumExp (OpNum  _ x y) = do checkNumExp x
                                checkNumExp y
checkNumExp (Access v n) = do checkVecExp v
                              checkNumExp n
checkNumExp (Mean      x) = checkRandExp x
checkNumExp (Variance     x) = checkRandExp x
checkNumExp (StdDev     x) = checkRandExp x
checkNumExp (PDF    x n) = do checkContExp x
                              checkNumExp n
checkNumExp (MaxP     x) = checkDiscExp x
checkNumExp (MaxPDF   x) = checkContExp x
checkNumExp (Prob  x _ n) = do checkRandExp x
                               checkNumExp n
checkNumExp x@(ProbBetween {}) = checkProb x
checkNumExp (ProbStep x _ _ n) = do checkMarkovExp x
                                    checkNumExp n
checkNumExp (ProbPath x c) = do checkMarkovExp x
                                checkPathExp c
checkNumExp (ProbHit x _ _) = checkMarkovExp x
checkNumExp _            = throwErrorT NumExpExpected


-- =====================================================
-- | Vector Expression Checker                          
-- =====================================================        
checkVecExp :: MonadProb m => Exp -> m ()                       
checkVecExp (VarRef n) = lookTy n Vec                       
checkVecExp (Mode   x) = checkRandExp x                       
checkVecExp (ConstV v) = V.mapM_ checkNumExp v                        
checkVecExp (Stationary x) = checkMarkovExp x                         
checkVecExp _          = throwErrorT VecExpExpected                       


-- ======================================================
-- | Random Expression Checker
-- ======================================================
checkRandExp :: MonadProb m => Exp -> m ()
checkRandExp (VarRef       x) = lookRand x
checkRandExp (Rand (DiscE x)) = checkDiscExp' x      
checkRandExp (Rand (ContE x)) = checkContExp' x      
checkRandExp _       = throwErrorT RandExpExpected          
                                                      
-- | Discrete Expression
checkDiscExp :: MonadProb m => Exp -> m ()            
checkDiscExp (VarRef x) = lookTy x RandDisc              
checkDiscExp (Rand (DiscE x)) = checkDiscExp' x                
checkDiscExp _ = throwErrorT DiscExpExpected                                    
                                                      
-- | Continuous Expression
checkContExp :: MonadProb m => Exp -> m ()
checkContExp (VarRef x) = lookTy x RandCont
checkContExp (Rand (ContE x)) = checkContExp' x
checkContExp _ = throwErrorT ContExpExpected
                                                      
                                                      
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


-- ======================================================= 
-- | Node Expressions Checker                                  
-- ======================================================= 
checkNodeExp :: MonadProb m => NodeExp -> m () 
checkNodeExp (NE xs) = mapM_ (checkNumExp . snd) xs


-- =====================================================
-- | Path Expressions Checker                                    
-- ======================================================
checkPathExp :: MonadProb m => Exp -> m () 
checkPathExp (VarRef  c) = lookTy c Path 
checkPathExp (ConstCh x) = checkPath x 
checkPathExp (SimulFromName x _ n) = do checkMarkovExp x 
                                        checkNumExp n 
checkPathExp (SimulFromVec x v n) = do checkMarkovExp x 
                                       checkVecExp v 
                                       checkNumExp n 
checkPathExp _ = throwErrorT PathExpExpected 

-- ======================================================
-- | Path Value Checker                                       
-- ======================================================
checkPath ::MonadProb m => Path -> m ()
checkPath x | V.null x  = throwErrorT EmptyPath
            | otherwise = return ()


-- ======================================================
-- | Markov Expressions Checker                            
-- ======================================================
checkMarkovExp :: MonadProb m => Exp -> m () 
checkMarkovExp (VarRef  x) = lookTy x Mark
checkMarkovExp (Markov  (MarkovE x)) = do checkPath x
                                          lookNodes x
checkMarkovExp (NextDist x n) = do checkMarkovExp x
                                   checkNumExp n
checkMarkovExp _          = throwErrorT MkExpExpected

