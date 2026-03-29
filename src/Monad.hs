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
import PPrint
import Control.Monad.IO.Class
import qualified Data.Map as M
import Prettyprinter(Doc, Pretty (pretty))
import Prettyprinter.Render.Terminal
import Control.Monad.Writer
type Trace = Doc AnsiStyle


class ( MonadIO m
      , MonadState Env m
      , MonadError Error m
      , MonadReader Conf m
      , MonadWriter [Trace] m
      ) => MonadProb m where

type ProbM = ReaderT Conf (StateT Env (ExceptT Error (WriterT [Trace] IO)))

instance MonadProb ProbM

getDecls :: MonadProb m => m (M.Map Name Value)
getDecls = do e <- get
              case e of 
                  Env d _-> return d


getNodes :: MonadProb m => m (M.Map Name Node)
getNodes = do e <- get
              case e of 
                  Env _ n-> return n

getNode :: MonadProb m => Name -> m Node
getNode n = do s <- getNodes
               case M.lookup n s of
                  Just x -> return x
                  _             -> throwErrorE VarNotScoope


getNumC :: MonadProb m => Name -> m NumC
getNumC n = do s <- getDecls
               case M.lookup n s of
                Just (VNum e) -> return e
                Just _        -> throwErrorE InvalidVarType
                _             -> throwErrorE VarNotScoope

getVec :: MonadProb m => Name -> m (Vec NumC)
getVec n = do s <- getDecls
              case M.lookup n s of
                Just (VVec e) -> return e
                Just _        -> throwErrorE InvalidVarType
                _             -> throwErrorE VarNotScoope

getAle :: MonadProb m => Name -> m RandVar
getAle n = do s <- getDecls
              case M.lookup n s of
                Just (VAle e) -> return e
                Just _        -> throwErrorE InvalidVarType
                _             -> throwErrorE VarNotScoope

tellIfVerbose :: MonadProb m => Trace -> m ()
tellIfVerbose msg = do
  v <- asks verbose
  when v (tell [msg])


add ::MonadProb m => Name -> Value -> m ()
add x (VN n) = addNode x n
add x a      = addDecl x a


addDecl :: MonadProb m => Name -> Value -> m ()
addDecl x a = do
  modify (\s -> s {decls=M.insert x a (decls s), ndes=ndes s} )


addNode :: MonadProb m => Name -> Node -> m ()
addNode x a = do
  modify (\s -> s {decls=decls s, ndes=M.insert x a (ndes s)} )




throwErrorE :: MonadProb m => EError -> m a
throwErrorE e = throwError (ExecErr e)





runProbM :: ProbM a -> Conf -> Env -> IO (Either Error (a, Env), [Trace])
runProbM c conf env =
  runWriterT $ runExceptT $ runStateT (runReaderT c conf) env


printP :: (MonadProb m, Show a) => a -> m ()
printP x = liftIO (print x)