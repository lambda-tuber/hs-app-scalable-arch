{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Stop where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH

-- |
--
instanceTH_IAppState ''StopStateData

-- |
--
instance IStateActivity StopStateData EntryEventData where
  action _ _ = do
    liftIO $ putStrLn "Stop entry called."
    return Nothing

-- |
--
instance IStateActivity StopStateData ExitEventData where
  action _ _ = do
    liftIO $ putStrLn "Stop exit called."
    return Nothing

-- |
--
instance IStateActivity StopStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData InitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData LaunchEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData DisconnectEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StopStateData TerminateEventData where
  action _ _ = do
    liftIO $ putStrLn "Stop terminate called. will exit."
    return Nothing
