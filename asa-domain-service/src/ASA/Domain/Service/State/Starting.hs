{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Starting where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH
import ASA.Domain.Service.State.Starting.Init()
import ASA.Domain.Service.State.Starting.Launch()
import ASA.Domain.Service.State.Starting.Disconnect()
import ASA.Domain.Service.State.Starting.Terminate()

{-
-- |
--
instance IAppState StartingStateData where
  actionS s (RequestW r@EntryRequest{})      = action s r
  actionS s (RequestW r@ExitRequest{})       = action s r
  actionS s (RequestW r@TransitRequest{})    = action s r
  actionS s (RequestW r@InitRequest{})       = action s r
  actionS s (RequestW r@LaunchRequest{})     = action s r
  actionS s (RequestW r@DisconnectRequest{}) = action s r
  actionS s (RequestW r@TerminateRequest{})  = action s r
-}
instanceTH_IAppState ''StartingStateData

-- |
--
instance IStateActivity StartingStateData EntryRequestData where
  action _ _ = do
    liftIO $ putStrLn "Starting entry called."
    return Nothing

-- |
--
instance IStateActivity StartingStateData ExitRequestData where
  action _ _ = do
    liftIO $ putStrLn "Starting exit called."
    return Nothing

-- |
--
instance IStateActivity StartingStateData TransitRequestData
  -- @see default implementation in Type module.

