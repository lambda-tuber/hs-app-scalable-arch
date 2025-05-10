{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.App.Core where

import Control.Monad.Logger
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Lens
import Control.Exception
import System.IO

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.App.Type
import ASA.App.Utility

import Control.Concurrent.Async

-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."

  liftIOE $ hSetBuffering stderr LineBuffering

  domCtxs <- view appsAppData <$> ask
  domDat  <- lift ask

  liftIOE (start domDat domCtxs) >>= \case
    (_, Right _) -> $logInfoS DM._LOGTAG "some threads stopped. exit."
    (_, Left e)  -> $logErrorS DM._LOGTAG  $ T.pack $ "some threads stopped. " ++ show e

  where
    start :: DM.DomainData
          -> [DM.DomainContext ()]
          -> IO (Async (), Either SomeException ())
    start domDat domCtxs = do
      as <- mapM (go domDat) domCtxs
      waitAnyCatchCancel as

    go :: DM.DomainData -> DM.DomainContext () -> IO (Async ())
    go domDat domCtx = async $ domCtx domDat
