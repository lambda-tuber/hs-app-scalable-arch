{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.State.Stopping where

import Control.Monad.IO.Class

import ASA.Domain.Service.Type
import ASA.Domain.Service.TH

-- |
--
instanceTH_IAppState ''StoppingStateData

-- |
--
instance IStateActivity StoppingStateData EntryRequestData where
  action _ _ = do
    liftIO $ putStrLn "Stopping entry called."
    return Nothing

-- |
--
instance IStateActivity StoppingStateData ExitRequestData where
  action _ _ = do
    liftIO $ putStrLn "Stopping exit called."
    return Nothing

-- |
--
instance IStateActivity StoppingStateData TransitRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StoppingStateData InitRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StoppingStateData LaunchRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StoppingStateData DisconnectRequestData
  -- @see default implementation in Type module.

-- |
--
instance IStateActivity StoppingStateData TerminateRequestData where
  action _ _ = do
    liftIO $ putStrLn "Stopping terminate called. will exit."
    return Nothing
