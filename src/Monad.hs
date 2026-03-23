{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Monad where

import AST
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

type MonadProb m =
  ( MonadIO m
  , MonadReader Conf m
  , MonadState Env m
  , MonadError Error m
  , MonadWriter [Trace] m
  )

traceMsg :: MonadProb m => String -> m ()
traceMsg msg = do
  verbose <- asks verbose
  when verbose (tell [msg])

addDeclAle :: MonadProb m => Name -> VarAle -> m ()
addDeclAle x a = do
  traceMsg ("[SET ALE] " ++ x)
  modify (\s -> s { ale = M.insert x a (ale s) })

addDeclVec :: MonadProb m => Name -> Vec NumC -> m ()
addDeclVec x v = do
  traceMsg ("[SET VEC] " ++ x)
  modify (\s -> s { vec = M.insert x v (vec s) })


addDeclNum :: MonadProb m => Name -> NumC -> m ()
addDeclNum x n = do
  traceMsg ("[SET NUM] " ++ x)
  modify (\s -> s { num = M.insert x n (num s) })


removeAle :: MonadProb m => Name -> m ()
removeAle x = do
  traceMsg ("Removing ale declaration: " ++ x)
  modify (\s -> s { ale = M.delete x (ale s) })


removeVec :: MonadProb m => Name -> m ()
removeVec x = do
  traceMsg ("Removing vec declaration: " ++ x)
  modify (\s -> s { vec = M.delete x (vec s) })


removeNum :: MonadProb m => Name -> m ()
removeNum x = do
  traceMsg ("Removing num declaration: " ++ x)
  modify (\s -> s { num = M.delete x (num s) })





lookupAle :: MonadProb m => Name -> m (Maybe VarAle)
lookupAle nm = gets (lookup nm . ale)

lookupVec :: MonadProb m => Name -> m (Maybe (Vec NumC))
lookupVec nm = gets (lookup nm . vec)

lookupNum :: MonadProb m => Name -> m (Maybe NumC)
lookupNum nm = gets (lookup nm . num)


runProbM :: MonadProb m => m a -> Conf -> Env -> IO (Either Error (a, Env), [Trace])
runProbM m conf env = runWriterT $ runExceptT $ runStateT (runReaderT m conf) env


