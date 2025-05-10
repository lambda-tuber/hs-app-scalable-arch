{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Run where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH

instanceTH_IAppState ''RunStateData

-- |
--
instance IStateActivity RunStateData EntryEventData where
  action _ _ = do
    liftIO $ putStrLn "Run entry called."
    return Nothing

-- |
--
instance IStateActivity RunStateData ExitEventData where
  action _ _ = do
    liftIO $ putStrLn "Run exit called."
    return Nothing

-- |
--
instance IStateActivity RunStateData TransitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData InitEventData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunStateData LaunchEventData where
  action _ _ = do
    liftIO $ putStrLn "Run launch called."
    return Nothing

-- |
--
instance IStateActivity RunStateData DisconnectEventData where
  action _ _ = do
    liftIO $ putStrLn "Run discoonect called."
    return $ Just RunToStop

-- |
--
instance IStateActivity RunStateData TerminateEventData
  -- @see default implementation in Type module.
