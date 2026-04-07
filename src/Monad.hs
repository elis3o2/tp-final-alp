{-|
Module      : Monad
Description : MonadProb definition
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Monad where

import AST
import Common
import Global
import Error
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as M
import System.Random (randomRIO)
import qualified Data.Vector as V



class ( MonadIO m
      , MonadState Env m
      , MonadError Error m
      , MonadReader Conf m
      ) => MonadProb m where
  getRandom :: m Double
      

type ProbM = ReaderT Conf (StateT Env (ExceptT Error IO))

instance MonadProb ProbM where
  getRandom = liftIO (randomRIO (0.0, 1.0))


throwErrorE :: MonadProb m => EError -> m a
throwErrorE e = throwError (ExecErr e)


throwErrorT :: MonadProb m => TError -> m a
throwErrorT e = throwError (TypeErr e)


getDecls :: MonadProb m => m (M.Map Name (Value,Type))
getDecls = gets decls

getNodes :: MonadProb m => m (M.Map Name NodeVal)
getNodes = gets nodes 


getNode :: MonadProb m => Name -> m NodeVal
getNode n = do s <- getNodes
               case M.lookup n s of
                  Just x -> return x
                  _             -> throwErrorE TypeCheckError



getNum :: MonadProb m => Name -> m Double
getNum n = do s <- getDecls
              case M.lookup n s of
                Just (VNum e, _) -> return e
                _             -> throwErrorE TypeCheckError

getVec :: MonadProb m => Name -> m (Vec Double)
getVec n = do s <- getDecls
              case M.lookup n s of
                Just (VVec e, _) -> return e
                _             -> throwErrorE TypeCheckError

getRand :: MonadProb m => Name -> m RandVar
getRand n = do s <- getDecls
               case M.lookup n s of
                 Just (VRand e, _) -> return e
                 _             -> throwErrorE TypeCheckError


getPath :: MonadProb m => Name -> m Path
getPath n = do s <- getDecls
               case M.lookup n s of
                  Just (VPath e, _) -> return e
                  _                 -> throwErrorE TypeCheckError
 

getMarkov :: MonadProb m => Name -> m Markov
getMarkov n = do s <- getDecls
                 case M.lookup n s of
                  Just (VMark e, _) -> return e
                  _             -> throwErrorE TypeCheckError



addDummyDecl :: MonadProb m => Name -> Type -> m ()
addDummyDecl x t = do
  modify (\s -> s {decls=M.insert x (Dummy, t) (decls s)} )


updateDecl :: MonadProb m => Name -> Value -> m ()
updateDecl x val = do declsMap <- getDecls 
                      case M.lookup x declsMap of 
                        Just (_, t) ->
                          modify (\s -> s { decls = M.insert x (val, t) declsMap })
                        Nothing ->
                          throwErrorT VarNotScope

addNode :: MonadProb m => Name -> NodeVal -> m ()
addNode x a = do
  modify (\s -> s {nodes=M.insert x a (nodes s)} )

addNodeDummy :: MonadProb m => Name  -> m ()
addNodeDummy x = do
  modify (\s -> s {nodes=M.insert x (N []) (nodes s)} )



lookRand :: MonadProb m => Name  -> m ()
lookRand n = do d <- getDecls
                case M.lookup n d of 
                  Just (_,RandCont) -> return ()
                  Just (_,RandDisc) -> return ()
                  Just _            -> throwErrorT RandExpExpected
                  Nothing           -> throwErrorT VarNotScope


lookTy :: MonadProb m => Name -> Type -> m ()
lookTy n t = do d <- getDecls
                case M.lookup n d of 
                  Just (_,ty) -> if ty == t then return ()
                                 else throwErrorT InvalidVarType
                  Nothing     -> throwErrorT VarNotScope


getTy :: MonadProb m => Name -> m Type
getTy n = do d <- getDecls
             case M.lookup n d of 
                Just (_,ty) -> return ty
                Nothing     -> throwErrorT VarNotScope



lookNodes :: MonadProb m => Path -> m ()
lookNodes c = V.mapM_ getNode c




printP :: (MonadProb m, Show a) => a -> m ()
printP x = liftIO (print x)


runProbM :: ProbM a -> Conf -> Env -> IO (Either Error (a, Env))
runProbM c conf env =
  runExceptT $ runStateT (runReaderT c conf) env