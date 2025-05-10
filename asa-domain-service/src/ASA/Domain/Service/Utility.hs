{-# LANGUAGE OverloadedStrings #-}

module ASA.Domain.Service.Utility where

import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Reader
import System.Log.FastLogger
import qualified Control.Exception.Safe as E

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM
import ASA.Domain.Service.Type


-- |
--
changeTo :: AppStateW -> AppStateContext ()
changeTo nextSt = do
  curSt <- get
  _ <- actionSW curSt (EventW ExitEvent)

  let req = EventW EntryEvent
  _ <- actionSW nextSt req

  modify (\_ -> nextSt)


-- |
--
runAppState :: DM.DomainData -> AppStateW -> TimedFastLogger -> AppStateContext a -> IO (Either DM.ErrorData (a, AppStateW))
runAppState domDat st logger ctx =
  DM.runFastLoggerT domDat logger
    $ runExceptT
    $ flip runReaderT domDat
    $ runStateT ctx st

-- |
--
liftIOE :: IO a -> AppStateContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go x = E.catchAny (Right <$> x) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show


{-

-- |
--
runAppState :: DomainData -> AppStateContext a -> AppStateW -> IO (Either ErrorData (a, AppStateW))
runAppState appDat app state =
  runFastLoggerT appDat
    $ runExceptT
    $ flip runReaderT appDat
    $ runStateT app state

-- |
--
runFastLoggerT :: DomainData -> LoggingT IO a -> IO a
runFastLoggerT appDat app = do
  let logLevel = appDat^.logLevelAppData
      logger   = appDat^.loggerAppData
      
  runLoggingT (filterLogger (filterByLevel logLevel) app) $ output logger 

  where
    -- |
    --
    output :: TimedFastLogger
           -> Loc
           -> LogSource
           -> LogLevel
           -> LogStr
           -> IO ()
    output logger a b c d = do
      let msg = defaultLogStr a b c d
      logger (\ts -> toLogStr ts <> " " <> msg)

    -- |
    --
    filterByLevel :: LogLevel -> LogSource -> LogLevel -> Bool
    filterByLevel target _ actual = actual >= target

-- |
--
liftIOE :: IO a -> DomainContext a
liftIOE f = liftIO (go f) >>= liftEither
  where
    go :: IO b -> IO (Either String b)
    go f = E.catchAny (Right <$> f) errHdl

    errHdl :: E.SomeException -> IO (Either String a)
    errHdl = return . Left . show
-}