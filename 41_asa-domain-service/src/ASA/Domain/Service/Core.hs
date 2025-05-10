{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module ASA.Domain.Service.Core where

import Control.Monad.Logger
import Data.Conduit
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Control.Monad.Trans.State.Lazy

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH
import ASA.Domain.Service.State.Start()
import ASA.Domain.Service.State.Run()
import ASA.Domain.Service.State.Stop()

-- |
--
funcTH_transit


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
src :: ConduitT () String AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext String
    go = do
      queue <- view DM.requestQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT String EventW AppContext ()
work = await >>= \case
  Just reqBS -> lift (go reqBS) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work
  where
    go :: String -> AppContext EventW
    go str = return $ EventW (InitEvent (InitEventData str))

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT EventW Void AppContext ()
sink = await >>= \case
  Just ev -> lift (go ev) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: EventW -> AppContext ()
    go ev = get >>= flip actionSW ev >>= \case
      Nothing -> return ()
      Just st -> transit st
