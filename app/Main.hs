module Main where

import AST
import Global
import Error
import Monad

import qualified Parser as P

import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import Control.Exception (IOException, try)
import Control.Monad (void, when)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Control.Monad.IO.Class


data CLI = CLI
  { cliVerbose :: Bool
  , cliFiles    :: [FilePath]
  }

cliParser :: Parser (Bool, [FilePath])
cliParser = switch (long "verbose" <> short 'v' <> help "Modo verbose (mostrar traza de ejecución)")
  <*> some (strArgument (metavar "FILES..."))

main :: IO ()
main = do
  (verboseFlag, files) <- execParser opts
  mapM_ (compileFile verboseFlag) files
  where
    opts = info (cliParser <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2025" )


--------------------------------------------------------------------------------
-- Ejecución de archivos
--------------------------------------------------------------------------------

compileFile :: Bool -> FilePath -> IO ()
compileFile verboseFlag file = do
  econtent <- readFileSafe file
  case econtent of
    Left msg -> hPrint stderr msg
    Right content ->
      case P.parseProgram content of
        Left perr -> hPrint stderr perr
        Right comms -> do
          let conf = defaultConf { verbose = verboseFlag }
              env  = initialEnv

          (result, logs) <- runProbM (runProgram comms) conf env

          when verboseFlag $
            mapM_ putStrLn logs

          case result of
            Left err       -> hPrint stderr err
            Right (_ , _ ) -> return ()

readFileSafe :: FilePath -> IO (Either String String)
readFileSafe file = do
  r <- try (readFile file) :: IO (Either IOException String)
  case r of
    Left e  -> return (Left ("No se pudo abrir el archivo " ++ file ++ ": " ++ show e))
    Right s -> return (Right s)


main :: IO ()
main = execParser opts >>= go
  where 
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2025" )



go :: (Bool, [FilePath]) -> IO ()
go (verboseFlag, files) = 
  runOrFail (Conf optFlag m verboseFlag) $ mapM_ compileFile  files

runOrFail :: Conf -> MonadProb a -> IO a
runOrFail c m = do
  r <- runProbM m c
  case r of
    Left err -> do
      hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v


loadFile ::  MonadProb m => FilePath -> m [Comm]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    parseIO filename program x



handleComm :: MonadFD4 m => SDecl STerm -> m ()
handleComm (Let n e ty) = case ty of
      TAle -> do 
            e' <- evalExpAle
            addDeclAle n e'
            removeNum n
            removeVec n
      TNum -> do
           e' <- evalExpNum
           addDeclNum n e' 
           removeAle n
           removeVec n
      TVec -> do
           e' <- evalExpNum
           addDeclVec n e' 
           removeAle n
           removeNum n
handleComm (Print n e ty) = case ty of
        TAle -> do 
              e' <- evalExpAle
              
        TNum -> do
              e' <- evalExpNum
        TVec -> do
              e' <- evalExpNum



