{-|
Module      : Main
Description : Entry point. Parses CLI arguments, loads and compiles files.
              When verbose mode is enabled, prints each command and its result.
              When no files are provided, starts an interactive REPL.
-}

module Main where

import AST
import Control.Monad.Trans
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn )
import Control.Monad.Except (throwError)
import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Options.Applicative
import PPrint
import Global
import Error
import Parse
import Eval
import Monad
import TypeChecker
import Table
import Prettyprinter.Render.Terminal (putDoc)
import Plot
import qualified Data.Map as M
import System.Console.Haskeline
  ( runInputT, getInputLine, 
   defaultSettings, Settings(..)
  , InputT )

-- ============================================================
-- | CLI
-- ============================================================

parseMode :: Parser Bool
parseMode =
  switch (long "verbose" <> short 'v' <> help "Verbose mode")

parseDecimals :: Parser Int
parseDecimals =
  option auto
    ( long "decimals" <> short 'd' <> metavar "INT"
   <> help "Number of decimals to show"
   <> value 6 <> showDefault )

parseArgs :: Parser (Bool, Int, [FilePath])
parseArgs = (,,) <$> parseMode <*> parseDecimals <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Prob runner"
     <> header "Probability distributions and Markov chains" )


go :: (Bool, Int, [FilePath]) -> IO ()
go (opt, decs, [])    = repl (Conf opt decs)
go (opt, decs, files) = runOrFail (Conf opt decs) $ mapM_ compileFile files


runOrFail :: Conf -> ProbM a -> IO a
runOrFail c m = do
  res <- runProbM m c initialEnv
  case res of
    Left err -> do
      hPrint stderr err
      exitWith (ExitFailure 1)
    Right (v, _) -> return v


-- ============================================================
-- | Compilation
-- ============================================================

loadFile :: MonadProb m => FilePath -> m [Comm]
loadFile f = do
  let filename = reverse (dropWhile isSpace (reverse f))
  x <- liftIO $ catch (readFile filename)
         (\e -> do let err = show (e :: IOException)
                   hPutStrLn stderr ("Couldn't open the file " ++ filename ++ ": " ++ err)
                   return "")
  parseIO filename program x

compileFile :: MonadProb m => FilePath -> m ()
compileFile f = do
  comms <- loadFile f
  mapM_ checkComm comms
  mapM_ handleComm comms

parseIO :: MonadProb m => String -> P a -> String -> m a
parseIO filename p x =
  case runP p x filename of
    Left e  -> throwError (ParseErr e)
    Right r -> return r


-- ============================================================
-- | REPL
-- ============================================================

banner :: String
banner = unlines
  [ "┌─────────────────────────────────────┐"
  , "│  Prob - Probability Language REPL   │"
  , "│  :help for commands, :q to exit     │"
  , "└─────────────────────────────────────┘"
  ]

helpText :: String
helpText = unlines
  [ "Commands:"
  , "  :q / :quit         Exit the REPL"
  , "  :help              Show this message"
  , "  :env               Show all declared variables"
  , "  :load <file>       Load and execute a file"
  , "  <command>          Evaluate a command"
  ]


repl :: Conf -> IO ()
repl conf = do
  putStr banner
  runInputT settings (loop initialEnv)
  where
    settings = defaultSettings { historyFile = Just ".prob_history" }

    loop :: Env -> InputT IO ()
    loop env = do
      minput <- getInputLine "prob> "
      case minput of
        Nothing      -> liftIO $ putStrLn "Bye!"
        Just ":q"    -> liftIO $ putStrLn "Bye!"
        Just ":quit" -> liftIO $ putStrLn "Bye!"
        Just ""      -> loop env
        Just input   -> do
          env' <- liftIO $ handleInput conf env input
          loop env'

handleInput :: Conf -> Env -> String -> IO Env
handleInput conf env (':':rest) = handleReplComm conf env (trim rest)
handleInput conf env input      = handleLine     conf env input

handleLine :: Conf -> Env -> String -> IO Env
handleLine conf env input = do
  res <- runProbM action conf env
  case res of
    Left err      -> hPrint stderr err >> return env  -- error → env sin cambios
    Right (_, env') -> return env'
  where
    action = do
      comm <- parseIO "<interactive>" parseComm input
      checkComm comm
      handleComm comm

handleReplComm :: Conf -> Env -> String -> IO Env
handleReplComm conf env "help" = putStr helpText >> return env
handleReplComm conf env "env"  = printEnvIO env  >> return env
handleReplComm conf env rest
  | "load " `isPrefixOf` rest = do
      res <- runProbM (compileFile (trim (drop 5 rest))) conf env
      case res of
        Left err        -> hPrint stderr err >> return env
        Right (_, env') -> return env'
handleReplComm _    env cmd = do
  putStrLn ("Unknown command: :" ++ cmd)
  return env

printEnvIO :: Env -> IO ()
printEnvIO env =
  if M.null (decls env) && M.null (nodes env)
    then putStrLn "<empty environment>"
    else do
      mapM_ (\(n,(_,t)) -> putStrLn (n ++ " : " ++ show t)) (M.toList (decls env))
      mapM_ (\(n,_)     -> putStrLn (n ++ " : Node"))        (M.toList (nodes env))


isPrefixOf :: String -> String -> Bool
isPrefixOf []     _      = True
isPrefixOf _      []     = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys



printEnv :: MonadProb m => m ()
printEnv = do
  ds <- getDecls
  ns <- getNodes
  if M.null ds && M.null ns
    then liftIO $ putStrLn "<empty environment>"
    else do
      mapM_ printDecl (M.toList ds)
      mapM_ printNode (M.toList ns)
  where
    printDecl (n, (_, t)) = liftIO $ putStrLn (n ++ " : " ++ show t)
    printNode (n, _)      = liftIO $ putStrLn (n ++ " : Node")


trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- ============================================================
-- | Commands Handler
-- ============================================================

handleComm :: MonadProb m => Comm -> m ()

handleComm c@(Let x e) = do pc <- ppComm c
                            v  <- eval e
                            pv <- ppValue v
                            updateDecl x v
                            printVerbose pc
                            printVerbose pv
                            
handleComm c@(Print e) = do pc <- ppComm c
                            v  <- eval e
                            pv <- ppValue v
                            printVerbose pc
                            liftIO $ putDoc pv

handleComm c@(Table x) = do pc <- ppComm c
                            x' <- eval x
                            px <- ppValue x'
                            t  <- makeTable x'
                            printVerbose pc
                            printVerbose px
                            liftIO $ putStrLn t

handleComm c@(TableR x n m) = do pc <- ppComm c
                                 x' <- eval x
                                 n' <- eval n
                                 m' <- eval m
                                 t  <- makeTableR x' n' m'
                                 px <- ppValue x' 
                                 pn <- ppValue n'
                                 pm <- ppValue m'
                                 printVerbose pc
                                 printVerbose px
                                 printVerbose pn
                                 printVerbose pm                                 
                                 liftIO $ putStrLn t

handleComm c@(Plot x) = do pc <- ppComm c
                           x' <- eval x
                           px <- ppValue x'
                           printVerbose pc
                           printVerbose px
                           case x' of
                             VRand v -> liftIO (plotRand v)
                             VMark v -> liftIO (plotMarkov v)
                             _       -> return ()

handleComm c@(LetN x e) = do pc <- ppComm c
                             e' <- evalNodeExp e
                             pe <- ppNode e'
                             addNode x e'
                             printVerbose pc
                             printVerbose pe