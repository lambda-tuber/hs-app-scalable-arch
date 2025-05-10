{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.Infra.Core where

import System.IO
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Lens
import Control.Monad.Reader
import qualified Control.Concurrent.STM as STM
import Data.Conduit
import Control.Concurrent.Async

import qualified Control.Concurrent as C

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.Infra.Type
import ASA.Infra.Utility


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
src :: ConduitT () DM.Command AppContext ()
src = lift go >>= yield >> src
  where
    go :: AppContext DM.Command
    go = do
      queue <- view DM.commandQueueDomainData <$> lift ask
      liftIO $ STM.atomically $ STM.readTQueue queue

---------------------------------------------------------------------------------
-- |
--
work :: ConduitT DM.Command (IOTask ()) AppContext ()
work = await >>= \case
  Just cmd -> lift (go cmd) >>= yield >> work
  Nothing -> do
    $logWarnS DM._LOGTAG "work: await returns nothing. skip."
    work

  where
    go :: DM.Command -> AppContext (IOTask ())
    go (DM.InitializeCommand dat) = return $ task dat
    go _ = do
      $logDebugS DM._LOGTAG "work.go: not yet implemented."
      undefined

    task :: DM.InitializeCommandData -> IO ()
    task dat = do
      let params = dat^.DM.paramsInitializeCommandData
          callback = dat^.DM.callbackInitializeCommandData

      hPutStrLn stderr $ "[INFO] ASA.Infra.Core.work.task start. wait " ++ show params ++ " sec."
      C.threadDelay (params * 1000 * 1000)
      hPutStrLn stderr "[INFO] ASA.Infra.Core.work.task end."
      callback params

---------------------------------------------------------------------------------
-- |
--
sink :: ConduitT (IO ()) Void AppContext ()
sink = await >>= \case
  Just req -> lift (go req) >> sink
  Nothing -> do
    $logWarnS DM._LOGTAG "sink: await returns nothing. skip."
    sink

  where
    go :: (IO ()) -> AppContext ()
    go t = do
      _ <- liftIOE $  async t
      return ()


