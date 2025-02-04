{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de Prolog.
-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

import Control.Monad.Trans
import Data.List (isPrefixOf, intercalate )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn, hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering) )
import Data.Maybe (isNothing)

import System.Exit ( exitWith, ExitCode(ExitFailure) )
import Options.Applicative

import Parse ( P, runP, program, expr )
import Unify (runUnifier, solveQuery)
import PPrint (ppExpr, ppResult, ppResultsTree)
import Global
import Errors
import Lang
import MonadPL


prompt :: String
prompt = "> "

-- | Parser de banderas
parseMode :: Parser (Mode,Bool)
parseMode = (,) <$>
      (flag' Debug ( long "debug" <> short 'd' <> help "Imprime a medida que se ejecuta")
      <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
      <|> flag Eval        Eval        (long "eval" <> short 'e' <> help "Evaluar programa")
      )
   <*> pure False

-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode, Bool, [FilePath])
parseArgs = (\(a,b) c -> (a,b,c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Iterprete de Prolog"
     <> header "Interprete de Prolog para el Tp final de ALP" )

    go :: (Mode,Bool,[FilePath]) -> IO ()
    go (Interactive,opt,files) =
              runOrFail (Conf opt Interactive) (runInputT defaultSettings (repl files))
    go (m,opt, files) =
              runOrFail (Conf opt m) $ mapM_ compileFile files

runOrFail :: Conf -> PL a -> IO a
runOrFail c m = do
  r <- runPL m c
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadPL m, MonadMask m) => [FilePath] -> InputT m ()
repl args = do
       lift $ setInter True
       lift $ catchErrors $ mapM_ compileFile args
       s <- lift get
       when (inter s) $ liftIO $ putStrLn
         (  "Entorno interactivo para Prolog.\n"
         ++ "Escriba :? para recibir ayuda.")
       loop
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c
                       maybe loop (`when` loop) b

loadFile ::  MonadPL m => FilePath -> m [Expr]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    setLastFile filename
    parseIO filename program x

compileFile ::  MonadPL m => FilePath -> m ()
compileFile f = do
    i <- getInter
    m <- getMode
    setTrace $ m == Debug 
    when i $ printFD4 ("Abriendo "++f++"...")
    expresions <- loadFile f
    mapM_ handleExpr expresions
    setInter i

handleExpr ::  MonadPL m => Expr -> m ()
handleExpr q@(Query tree) = do
        m <- getMode
        case m of
          Interactive -> do
              printFD4 $ ppExpr q
              glb <- get
              opt <- getOpt 
              let a = runUnifier solveQuery glb q
              printResultInteractive a 1
          _ -> do
              printFD4 $ ppExpr q
              glb <- get
              let a = runUnifier solveQuery glb q
              printResultInteractive a 2
handleExpr e = do 
  --printFD4 $ ppExpr e
  addClause e

printResultInteractive' :: (MonadPL m) => Int -> ResultsTree -> m Int
printResultInteractive' 0 (RLeaf x) = return 0
printResultInteractive' 1 (RLeaf x) = if isNothing x then return 1 else do 
  liftIO . putStr $ ppResult x
  liftIO $ hSetBuffering stdin NoBuffering  -- Set no buffering, so input is immediately available
  liftIO $ hSetEcho stdin False
  minput <- liftIO getChar --getInputLine prompt
  case minput of
      '\n' -> return 0
      ' '  -> return 1
      'a'  -> return 2
      c    -> return 0
printResultInteractive' 2 (RLeaf x) = if isNothing x then return 2 else do 
  liftIO . putStr $ ppResult x
  return 2
printResultInteractive' _ (RLeaf x) = return 0
printResultInteractive' skip (RNode xs) = foldM printResultInteractive' skip xs

printResultInteractive :: (MonadPL m) => ResultsTree -> Int -> m ()
printResultInteractive result mode = do 
  a <- printResultInteractive' mode result 
  case a of
    0 -> return ()
    _ -> printFD4 "false."

parseIO ::  MonadPL m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

------------------------------------------
-- Interactivo
------------------------------------------

data Command = Compile CompileForm
             | PPrint String
             | Trace Bool
             | Reload
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if ":" `isPrefixOf` x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   intercalate ", " ([ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

aux :: String -> Bool
aux "on" = True
aux "off" = False
aux _ = False

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   PPrint          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":reload"]      ""        (const Reload)         "Vuelve a cargar el último archivo cargado",
       Cmd [":trace"]       "<on/off>" (Trace . aux)        "Activa o desactiva la traza para debug",
       Cmd [":quit",":Q"]   ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = intercalate ", " (map (++ if null a then "" else " " ++ a) c)
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadPL m => Command -> m Bool
handleCommand cmd = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printFD4 (helpTxt commands) >> return True
       Browse ->  return True --do  printFD4 (unlines (reverse (nub (map declName glb))))
                              --  return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e
                          CompileFile f        -> compileFile f
                      return True
       Reload ->  return True -- eraseLastFileDecls >> (getLastFile >>= compileFile) >> return True
       PPrint e   -> do 
        e' <- parseIO "<interactive>" expr e
        printFD4 (ppExpr e') >> return True
       Trace b    -> setTrace b  >> return True -- typeCheckPhrase e >> return True

compilePhrase ::  MonadPL m => String -> m ()
compilePhrase x = do
    dot <- parseIO "<interactive>" expr x
    handleExpr dot
