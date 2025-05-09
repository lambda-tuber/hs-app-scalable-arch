{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.UI.Response.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString.Lazy as B
import Data.Conduit

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.UI.Response.Type


-- |
--
app :: AppContext ()
app = do
  $logDebugS DM._LOGTAG "app called."
  runConduit pipeline
  where
    pipeline :: ConduitM () Void AppContext ()
    pipeline = src .| work .| sink

---------------------------------------------------------------------------------
-- |
--
src :: ConduitT () Int AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext Int
    go = do
      queue <- view DM.responseQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT Int B.ByteString AppContext ()
work = await >>= \case
  Just reqBS -> lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work

  where
    go :: Int -> AppContext B.ByteString
    go x = return . DM.str2lbs . show $ x

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT B.ByteString Void AppContext ()
sink = await >>= \case
  Just req -> lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: B.ByteString -> AppContext ()
    go bs = do
      hdl <- view outputHandleAppData <$> ask
      liftIO $ B.hPutStr hdl bs
      liftIO $ B.hPutStr hdl "\n"
      liftIO $ hFlush hdl
