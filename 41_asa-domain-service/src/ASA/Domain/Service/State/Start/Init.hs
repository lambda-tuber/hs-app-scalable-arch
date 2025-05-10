{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module ASA.Domain.Service.State.Start.Init where

import System.IO
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Lens
import qualified Data.Text as T
import qualified Text.Read as T
import qualified Control.Concurrent.STM as STM

import qualified ASA.Domain.Model.Type as DM
import qualified ASA.Domain.Model.Constant as DM

import ASA.Domain.Service.Type


-- |
--
instance IStateActivity StartStateData InitEventData where
  action s (InitEvent r@(InitEventData str)) = do
    $logDebugS DM._LOGTAG "Start init called."
    $logDebugS DM._LOGTAG (T.pack (show s))
    $logDebugS DM._LOGTAG (T.pack (show r))

    case T.readMaybe str of
      Just x  -> runCommand x 
      Nothing -> goResponse

    where
      goResponse = do
        let len = length str
        queue <- view DM.responseQueueDomainData <$> lift ask
        liftIO $ STM.atomically $ STM.writeTQueue queue len

        return $ Just StartToRun
      
      runCommand x = do
        resQ  <- view DM.responseQueueDomainData <$> lift ask
        queue <- view DM.commandQueueDomainData <$> lift ask
        let cmdDat = DM.InitializeCommandData {
                     DM._paramsInitializeCommandData = x
                   , DM._callbackInitializeCommandData = initCallback resQ
                   }
            cmd = DM.InitializeCommand cmdDat
        liftIO $ STM.atomically $ STM.writeTQueue queue cmd

        return Nothing
      
      -- |
      --
      initCallback :: STM.TQueue Int -> Int -> IO ()
      initCallback queue x = do
        hPutStrLn stderr $ "[INFO] ASA.Domain.Service.State.Start.Init.action.initCallback called."
        hPutStrLn stderr $ "[INFO] result is " ++  show x ++ "."
        STM.atomically $ STM.writeTQueue queue x

