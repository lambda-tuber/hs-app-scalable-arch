{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Start.Disconnect where

import ASA.Domain.Service.Type

-- |
--
instance IStateActivity StartStateData DisconnectEventData
  -- @see default implementation in Type module.

