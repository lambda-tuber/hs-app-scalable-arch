{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Start.Terminate where

import ASA.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData TerminateEventData
  -- @see default implementation in Type module.

