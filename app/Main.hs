{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental
-}

module Main where
import AST
import Control.Monad.Trans
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn )
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Options.Applicative
import PPrint
import Global
import Error
import Parse 
import Eval 
import Monad
import TypeChecker 
import Debug.Trace
import Monad (MonadProb, ProbM, runProbM)
import Table
import Data.Bits (Bits(xor))

-- | Parser de banderas
parseMode :: Parser Bool
parseMode =
  switch (long "verbose" <> short 'v' <> help "Modo verbose")

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Bool, [FilePath])
parseArgs = (\a b -> (a,b)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2025" )

    go :: (Bool,[FilePath]) -> IO ()
    go (opt, files) =
              runOrFail (Conf opt) $ mapM_ compileFile files


runOrFail :: Conf -> ProbM a -> IO a
runOrFail c m = do
  (res, _) <- runProbM m c initialEnv
  case res of
    Left err -> do
      hPrint stderr err
      exitWith (ExitFailure 1)
    Right (v, _) -> return v

loadFile ::  MonadProb m => FilePath -> m [Comm]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    parseIO filename program x

compileFile ::  MonadProb m => FilePath -> m ()
compileFile f = do
    comms <- loadFile f
    liftIO (print comms)
    mapM_ checkComm comms
    mapM_ handleComm comms


parseIO :: MonadProb m => String -> P a -> String -> m a
parseIO filename p x =
  case runP p x filename of
    Left e  -> throwError (ParseErr e)
    Right r -> return r


handleComm :: MonadProb m => Comm -> m ()
handleComm (Let x e) = do v <- eval e
                          updateDecl x v
handleComm (Print e) = do _ <- liftIO (print e)
                          v <- eval e
                          liftIO $ print v
handleComm (Table x)= do x' <- eval x
                         t <- makeTable x'
                         liftIO $ putStrLn t
handleComm (TableR x n m) = do x' <- eval x
                               n' <- eval n
                               m' <- eval m
                               t <- makeTableR x' n' m'
                               liftIO $ putStrLn t
handleComm (Plot x) = do x' <- eval x 
                         case x' of
                          VRand (Disc v) -> liftIO (plotDisc v) 
                          VRand (Cont v) -> liftIO (plotCont v)
                          _ -> return () 
handleComm (LetN x e) = do e' <- evalNodeExp e 
                           addNode x e'
