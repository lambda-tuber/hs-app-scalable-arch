{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Start.Launch where

import ASA.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData LaunchEventData
  -- @see default implementation in Type module.

