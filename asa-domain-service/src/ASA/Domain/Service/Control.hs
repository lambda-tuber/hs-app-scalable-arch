{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}


module ASA.Domain.Service.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM

import ASA.Domain.Service.Constant
import ASA.Domain.Service.Type
import ASA.Domain.Service.Core
import ASA.Domain.Service.State.Starting()
import ASA.Domain.Service.State.Running()
import ASA.Domain.Service.State.Stopping()

import ASA.Domain.Service.Utility


-- |
--
run :: DM.DomainContext ()
run dat = do
  hPutStrLn stderr "[INFO] ASA.Domain.Service.Control.run called."

  let appDat = AppStateW StartingState
  runWithAppData appDat dat

-- |
--
runWithAppData :: AppStateW -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppStateW -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runAppState domDat appDat logger app
    >>= \case
      Right (x, _) -> return x
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] ASA.Domain.Service.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.Domain.Service.Control.run exception occured."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.Domain.Service.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
