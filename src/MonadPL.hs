{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module MonadPL (
  PL,
  runPL,
  printFD4,
  setLastFile,
  getLastFile,
  setInter,
  getInter,
  setTrace,
  getMode,
  getOpt,
  eraseLastFileDecls,
  failPosFD4,
  failFD4,
  addClause,
  catchErrors,
  MonadPL,
  module Control.Monad.Except,
  module Control.Monad.State)
 where

import Common
import Lang
import Global
import Errors ( Error(..) )
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Reader
import System.IO

-- * La clase 'MonadPL'

{-| La clase de mónadas 'MonadPL' clasifica a las mónadas con soporte para una configuración Global 'Global.Conf', 
    para operaciones @IO@, estado de tipo 'Global.GlEnv', y errores de tipo 'Errors.Error'.

Las mónadas @m@ de esta clase cuentan con las operaciones:
   - @ask :: m Conf@
   - @get :: m GlEnv@
   - @put :: GlEnv -> m ()@
   - @throwError :: Error -> m a@
   - @catchError :: m a -> (Error -> m a) -> m a@
   - @liftIO :: IO a -> m a@

y otras operaciones derivadas de ellas, como por ejemplo
   - @modify :: (GlEnv -> GlEnv) -> m ()@
   - @gets :: (GlEnv -> a) -> m a@  
-}
class (MonadIO m, MonadState GlEnv m, MonadError Error m, MonadReader Conf m) => MonadPL m where

getOpt :: MonadPL m => m Bool
getOpt = asks opt

getMode :: MonadPL m => m Mode
getMode = asks modo

setInter :: MonadPL m => Bool -> m ()
setInter b = modify (\s-> s {inter = b})

setTrace :: MonadPL m => Bool -> m ()
setTrace b = modify (\s-> s {traceDebug = b})

getInter :: MonadPL m => m Bool
getInter = gets inter

printFD4 :: MonadPL m => String -> m ()
printFD4 = liftIO . putStrLn

setLastFile :: MonadPL m => FilePath -> m ()
setLastFile filename = modify (\s -> s {lfile = filename , cantClause = 0})

getLastFile :: MonadPL m => m FilePath
getLastFile = gets lfile

addClause :: MonadPL m => Expr -> m ()
addClause d = modify (\s -> s { glb = d : glb s, cantClause = cantClause s + 1 })

getClauses :: MonadPL m => m [Expr]
getClauses = gets glb

eraseLastFileDecls :: MonadPL m => m ()
eraseLastFileDecls = do
      s <- get
      let n = cantClause s
          (_,rem) = splitAt n (glb s)
      modify (\s -> s {glb = rem, cantClause = 0})

failPosFD4 :: MonadPL m => Pos -> String -> m a
failPosFD4 p s = throwError (ErrPos p s)

failFD4 :: MonadPL m => String -> m a
failFD4 = failPosFD4 NoPos

catchErrors  :: MonadPL m => m a -> m (Maybe a)
catchErrors c = catchError (Just <$> c)
                           (\e -> liftIO $ hPrint stderr e
                              >> return Nothing)


type PL = ReaderT Conf (StateT GlEnv (ExceptT Error IO))

-- | Esta es una instancia vacía, ya que 'MonadPL' no tiene funciones miembro.
instance MonadPL PL

-- 'runPL\'' corre una computación de la mónad 'PL' en el estado inicial 'Global.initialEnv' 
runPL' :: PL a -> Conf -> IO (Either Error (a, GlEnv))
runPL' c conf =  runExceptT $ runStateT (runReaderT c conf)  initialEnv

runPL:: PL a -> Conf -> IO (Either Error a)
runPL c conf = fmap fst <$> runPL' c conf
