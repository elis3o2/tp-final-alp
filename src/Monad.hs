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
import System.Random (randomRIO)
import qualified Data.Vector as V


type Trace = Doc AnsiStyle


class ( MonadIO m
      , MonadState Env m
      , MonadError Error m
      , MonadReader Conf m
      , MonadWriter [Trace] m
      ) => MonadProb m where
  getRandom :: m Double
      

type ProbM = ReaderT Conf (StateT Env (ExceptT Error (WriterT [Trace] IO)))

instance MonadProb ProbM where
  getRandom = liftIO (randomRIO (0.0, 1.0))


getDecls :: MonadProb m => m (M.Map Name (Value,Type))
getDecls = do e <- get
              case e of 
                  Env d _-> return d


getNodes :: MonadProb m => m (M.Map Name NodeVal)
getNodes = do e <- get
              case e of 
                  Env _ n-> return n

getNode :: MonadProb m => Name -> m NodeVal
getNode n = do s <- getNodes
               case M.lookup n s of
                  Just x -> return x
                  _             -> throwErrorE TypeCheckError



getNumC :: MonadProb m => Name -> m Double
getNumC n = do s <- getDecls
               case M.lookup n s of
                Just (VNum e, _) -> return e
                _             -> throwErrorE TypeCheckError

getVec :: MonadProb m => Name -> m (Vec Double)
getVec n = do s <- getDecls
              case M.lookup n s of
                Just (VVec e, _) -> return e
                _             -> throwErrorE TypeCheckError

getAle :: MonadProb m => Name -> m RandVar
getAle n = do s <- getDecls
              case M.lookup n s of
                Just (VRand e, _) -> return e
                _             -> throwErrorE TypeCheckError


getPath :: MonadProb m => Name -> m Path
getPath n = do s <- getDecls
                case M.lookup n s of
                 Just (VPath e, _) -> return e
                 _             -> throwErrorE TypeCheckError


getMarkov :: MonadProb m => Name -> m Markov
getMarkov n = do s <- getDecls
                 case M.lookup n s of
                  Just (VMark e, _) -> return e
                  _             -> throwErrorE TypeCheckError





tellIfVerbose :: MonadProb m => Trace -> m ()
tellIfVerbose msg = do
  v <- asks verbose
  when v (tell [msg])



addDummyDecl :: MonadProb m => Name -> Type -> m ()
addDummyDecl x t = do
  modify (\s -> s {decls=M.insert x (Dummy, t) (decls s), ndes=ndes s} )


updateDecl :: MonadProb m => Name -> Value -> m ()
updateDecl x val = do declsMap <- getDecls 
                      case M.lookup x declsMap of 
                        Just (_, t) ->
                          modify (\s -> s { decls = M.insert x (val, t) declsMap })
                        Nothing ->
                          throwErrorT VarNotScoope

addNode :: MonadProb m => Name -> NodeVal -> m ()
addNode x a = do
  modify (\s -> s {decls=decls s, ndes=M.insert x a (ndes s)} )

addNodeDummy :: MonadProb m => Name  -> m ()
addNodeDummy x = do
  modify (\s -> s {decls=decls s, ndes=M.insert x (N []) (ndes s)} )



throwErrorE :: MonadProb m => EError -> m a
throwErrorE e = throwError (ExecErr e)

throwErrorT :: MonadProb m => TError -> m a
throwErrorT e = throwError (TypeErr e)

lookRand :: MonadProb m => Name  -> m ()
lookRand n = do d <- getDecls
                case M.lookup n d of 
                  Just (_,RandCont) -> return ()
                  Just (_,RandDisc) -> return ()
                  Just _            -> throwErrorT RandExpExpected
                  Nothing           -> throwErrorT VarNotScoope


lookTy :: MonadProb m => Name -> Type -> m ()
lookTy n t = do d <- getDecls
                case M.lookup n d of 
                  Just (_,ty) -> if ty == t then return ()
                                 else throwErrorT InvalidVarType
                  Nothing     -> throwErrorT VarNotScoope



lookNodes :: MonadProb m => Path -> m ()
lookNodes c = do _ <- V.mapM getNode c
                 return ()

runProbM :: ProbM a -> Conf -> Env -> IO (Either Error (a, Env), [Trace])
runProbM c conf env =
  runWriterT $ runExceptT $ runStateT (runReaderT c conf) env


printP :: (MonadProb m, Show a) => a -> m ()
printP x = liftIO (print x)