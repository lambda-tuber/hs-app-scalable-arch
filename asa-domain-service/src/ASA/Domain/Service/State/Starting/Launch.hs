{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module ASA.Domain.Service.State.Starting.Launch where

import ASA.Domain.Service.Type


-- |
--
instance IStateActivity StartingStateData LaunchRequestData
  -- @see default implementation in Type module.

