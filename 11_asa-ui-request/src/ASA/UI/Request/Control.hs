module ASA.UI.Request.Control where

import System.IO
import qualified Control.Exception.Safe as E
import System.Log.FastLogger
import Data.Default

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM

import ASA.UI.Request.Type
import ASA.UI.Request.Constant
import ASA.UI.Request.Utility
import ASA.UI.Request.Core

-- |
--
run :: DM.DomainContext ()
run dat = do
  hPutStrLn stderr "[INFO] ASA.UI.Request.Control.run called."
  runWithAppData def dat

-- |
--
runWithAppData :: AppData -> DM.DomainContext ()
runWithAppData appDat domDat = do
  logDat <- DM.createLogger domDat _LOG_FILE_NAME
  runWithLogger logDat appDat domDat

-- |
--
runWithLogger :: (TimedFastLogger, IO ()) -> AppData -> DM.DomainContext ()
runWithLogger (logger, finalizeLogger) appDat domDat = 
  flip E.catchAny exception
    $ flip E.finally finalize
    $ runApp domDat appDat logger app
    >>= \case
      Right x -> return x
      Left  e -> errorEnd e

  where
    finalize = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[INFO] ASA.UI.Request.Control.run finalize called."
      finalizeLogger
      hPutStrLn stderr "-----------------------------------------------------------------------------"

    exception e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.UI.Request.Control.run exception occured."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      E.throwIO e

    errorEnd e = do
      hPutStrLn stderr "-----------------------------------------------------------------------------"
      hPutStrLn stderr "[ERROR] ASA.UI.Request.Control.run end with error."
      hPutStrLn stderr $ show e
      hPutStrLn stderr "-----------------------------------------------------------------------------"
