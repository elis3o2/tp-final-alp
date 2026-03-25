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
import Control.Monad.Writer
import Control.Monad.IO.Class
import Control.Monad (when)
import qualified Data.Map as M
type Trace = String

class ( MonadIO m
      , MonadState Env m
      , MonadError Error m
      , MonadReader Conf m
      , MonadWriter [Trace] m
      ) => MonadProb m where

type ProbM = ReaderT Conf (StateT Env (ExceptT Error (WriterT [Trace] IO)))

instance MonadProb ProbM


traceMsg :: MonadProb m => String -> m ()
traceMsg msg = do
  verbose <- asks verbose
  when verbose (tell [msg])

addDecl :: MonadProb m => Name -> Value -> m ()
addDecl x a = do
  traceMsg ("[SET ] " ++ x)
  modify (\s -> M.insert x a s )


throwErrorE :: MonadProb m => EError -> m a
throwErrorE e = throwError (ExecErr e)



lookupAle :: MonadProb m => Name -> m VarAle
lookupAle nm = do mv <- gets (M.lookup nm)
                  case mv of
                    Just (VAle x) ->  return x
                    Just _        -> throwErrorE InvalidVarType
                    Nothing       -> throwErrorE VarNotScoope

lookupVec :: MonadProb m => Name -> m (Vec NumC)
lookupVec nm = do mv <- gets (M.lookup nm)
                  case mv of
                    Just (VVec x) ->  return x
                    Just _        -> throwErrorE InvalidVarType
                    Nothing       -> throwErrorE VarNotScoope


lookupNum :: MonadProb m => Name -> m NumC
lookupNum nm = do mv <- gets (M.lookup nm)
                  case mv of
                    Just (VNum x) ->  return x
                    Just _        -> throwErrorE InvalidVarType
                    Nothing       -> throwErrorE VarNotScoope


runProbM :: ProbM a -> Conf -> Env -> IO (Either Error (a, Env), [Trace])
runProbM c conf env =
  runWriterT $ runExceptT $ runStateT (runReaderT c conf) env


printP :: (MonadProb m, Show a) => a -> m ()
printP x = liftIO (print x)