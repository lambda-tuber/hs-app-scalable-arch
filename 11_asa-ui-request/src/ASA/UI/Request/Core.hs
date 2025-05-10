{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.UI.Request.Core where

import Control.Monad.Logger
import Data.Conduit
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM


import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Utility as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.UI.Request.Type
import ASA.UI.Request.Utility

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
src :: ConduitT () B.ByteString AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext B.ByteString
    go = do
      hdl <- view inputHandleAppData <$> ask
      readLineL hdl

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT B.ByteString String AppContext ()
work = await >>= \case
  Just reqBS -> lift (go reqBS) >>= yield >> work
  Nothing  -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
    
  where
    go :: B.ByteString -> AppContext String
    go bs = return $ DM.lbs2str bs

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT String Void AppContext ()
sink = await >>= \case
  Just req -> lift (go req) >> sink
  Nothing  -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: String -> AppContext ()
    go str = do
      queue <- view DM.requestQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.writeTQueue queue str
