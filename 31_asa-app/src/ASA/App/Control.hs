{-# LANGUAGE LambdaCase #-}

module ASA.App.Control where

import qualified Control.Exception.Safe as E
import Data.Default
import Data.Yaml
import Control.Lens
import System.Log.FastLogger
import System.IO

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM

import ASA.App.Type
import ASA.App.Utility
import ASA.App.Core
import ASA.App.Constant


-- |
--
run :: ArgData
    -> [DM.DomainContext ()]
    -> IO ()
run args apps = do
  hPutStrLn stderr "[INFO] ASA.App.Control.run called."

  conf <- maybe (pure def) decodeFileThrow (args^.yamlArgData)
  defDom <- DM.defaultDomainData
  
  let domDat = defDom {
               DM._logDirDomainData   = conf^.logDirConfigData
             , DM._logLevelDomainData = conf^.logLevelConfigData
             }
      appDat = def {
               _appsAppData = apps
             }
  DM.createLogger domDat _LOG_FILE_NAME >>= runWithLogger domDat appDat


-- |
--
runWithLogger :: DM.DomainData -> AppData -> (TimedFastLogger, IO ()) -> IO ()
runWithLogger domDat appDat (logger, finalizeLogger) = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right _ -> return ()
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] ASA.App.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.App.Control.run exception occured."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.App.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
