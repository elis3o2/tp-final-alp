{-# LANGUAGE RecordWildCards #-}
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
import Monad (MonadProb, ProbM, runProbM)
import Table
import Prettyprinter (vsep, Doc)
import Prettyprinter.Render.Terminal (putDoc, AnsiStyle)
import Plot
import Control.Monad.Reader (asks)



-- ============================================================
-- | CLI
-- ============================================================
parseMode :: Parser Bool
parseMode =
  switch (long "verbose" <> short 'v' <> help "Modo verbose")

parseDecimals :: Parser Int
parseDecimals =
  option auto
    ( long "decimals" <> short 'd' <> metavar "INT"
   <> help "Cantidad de decimales a mostrar"
   <> value 6 <> showDefault )

parseArgs :: Parser (Bool, Int, [FilePath])
parseArgs = (,,) <$> parseMode <*> parseDecimals <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2025" )

go :: (Bool, Int, [FilePath]) -> IO ()
go (opt, decs, files) = runOrFail (Conf opt decs) $ mapM_ compileFile files


-- ============================================================
-- | Runner: captura e imprime las trazas acumuladas
-- ============================================================

runOrFail :: Conf -> ProbM a -> IO a
runOrFail c m = do
  res <- runProbM m c initialEnv
  case res of
    Left err -> do
      hPrint stderr err
      exitWith (ExitFailure 1)
    Right (v, _) -> return v





-- ============================================================
-- | Compilación
-- ============================================================

loadFile :: MonadProb m => FilePath -> m [Comm]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <- liftIO $ catch (readFile filename)
         (\e -> do let err = show (e :: IOException)
                   hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                   return "")
  parseIO filename program x

compileFile :: MonadProb m => FilePath -> m ()
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


-- ============================================================
-- | Handlers de comandos
-- ============================================================
handleComm :: MonadProb m => Comm -> m ()

handleComm (Let x e) = do v <- eval e
                          updateDecl x v

handleComm (Print e) = do v <- eval e
                          pv <- ppValue v
                          liftIO $ putDoc pv

handleComm (Table x) = do x' <- eval x
                          t  <- makeTable x'
                          liftIO $ putStrLn t

handleComm (TableR x n m) = do x' <- eval x
                               n' <- eval n
                               m' <- eval m
                               t  <- makeTableR x' n' m'
                               liftIO $ putStrLn t

handleComm (Plot x) = do x' <- eval x
                         case x' of
                           VRand v -> liftIO (plotRand v)
                           VMark v -> liftIO (plotMarkov v)
                           _       -> return ()

handleComm (LetN x e) = do e' <- evalNodeExp e
                           addNode x e'