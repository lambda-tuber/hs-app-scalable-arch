{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Start where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH
import ASA.Domain.Service.State.Start.Init()
import ASA.Domain.Service.State.Start.Launch()
import ASA.Domain.Service.State.Start.Disconnect()
import ASA.Domain.Service.State.Start.Terminate()

{-
-- |
--
instance IAppState StartStateData where
  actionS s (EventW r@EntryEvent{})      = action s r
  actionS s (EventW r@ExitEvent{})       = action s r
  actionS s (EventW r@TransitEvent{})    = action s r
  actionS s (EventW r@InitEvent{})       = action s r
  actionS s (EventW r@LaunchEvent{})     = action s r
  actionS s (EventW r@DisconnectEvent{}) = action s r
  actionS s (EventW r@TerminateEvent{})  = action s r
-}
instanceTH_IAppState ''StartStateData

-- |
--
instance IStateActivity StartStateData EntryEventData where
  action _ _ = do
    liftIO $ putStrLn "Start entry called."
    return Nothing

-- |
--
instance IStateActivity StartStateData ExitEventData where
  action _ _ = do
    liftIO $ putStrLn "Start exit called."
    return Nothing

-- |
--
instance IStateActivity StartStateData TransitEventData
  -- @see default implementation in Type module.

