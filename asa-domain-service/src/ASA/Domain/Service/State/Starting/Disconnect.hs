{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Starting.Disconnect where

import ASA.Domain.Service.Type

-- |
--
instance IStateActivity StartingStateData DisconnectRequestData
  -- @see default implementation in Type module.

