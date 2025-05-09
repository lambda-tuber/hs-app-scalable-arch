{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Running where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH

instanceTH_IAppState ''RunningStateData

-- |
--
instance IStateActivity RunningStateData EntryRequestData where
  action _ _ = do
    liftIO $ putStrLn "Running entry called."
    return Nothing

-- |
--
instance IStateActivity RunningStateData ExitRequestData where
  action _ _ = do
    liftIO $ putStrLn "Running exit called."
    return Nothing

-- |
--
instance IStateActivity RunningStateData TransitRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunningStateData InitRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity RunningStateData LaunchRequestData where
  action _ _ = do
    liftIO $ putStrLn "Running launch called."
    return Nothing

-- |
--
instance IStateActivity RunningStateData DisconnectRequestData where
  action _ _ = do
    liftIO $ putStrLn "Running discoonect called."
    return $ Just RunningToStopping

-- |
--
instance IStateActivity RunningStateData TerminateRequestData
  -- @see default implementation in Type module.
