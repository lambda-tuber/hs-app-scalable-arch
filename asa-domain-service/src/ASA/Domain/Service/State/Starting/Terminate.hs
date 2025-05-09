{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Starting.Terminate where

import ASA.Domain.Service.Type


-- |
--
instance IStateActivity StartingStateData TerminateRequestData
  -- @see default implementation in Type module.

