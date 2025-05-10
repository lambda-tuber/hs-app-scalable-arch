{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Model.Type where

import Control.Lens
import Data.Aeson
import Control.Monad.Logger
import qualified Data.Text as T
import qualified Text.Read as R
import Control.Concurrent.STM.TQueue


--------------------------------------------------------------------------------
-- |
--
instance FromJSON LogLevel where
  parseJSON (String v) = case R.readEither ("Level" ++ T.unpack v) of
    Right l -> pure l
    Left er -> error $ "invalid loglevel. <" ++ T.unpack v ++ "> " ++ er
  parseJSON o = error $ "json parse error. Priority:" ++ show o

instance ToJSON LogLevel where
  toJSON  LevelDebug    = String $ T.pack "Debug"
  toJSON  LevelInfo     = String $ T.pack "Info"
  toJSON  LevelWarn     = String $ T.pack "Warn"
  toJSON  LevelError    = String $ T.pack "Error"
  toJSON (LevelOther m) = String m


--------------------------------------------------------------------------------
-- |
--
data InitializeCommandData = InitializeCommandData {
    _paramsInitializeCommandData   :: Int
  , _callbackInitializeCommandData :: (Int -> IO ())
  }

makeLenses ''InitializeCommandData

data LaunchCommandData = LaunchCommandData

-- |
--
data Command =
    InitializeCommand InitializeCommandData
  | LaunchCommand     LaunchCommandData


--------------------------------------------------------------------------------
-- |
--
data DomainData = DomainData {
    _logDirDomainData       :: Maybe String
  , _logLevelDomainData     :: LogLevel
  , _requestQueueDomainData :: TQueue String
  , _responseQueueDomainData :: TQueue Int
  , _commandQueueDomainData :: TQueue Command
  }

makeLenses ''DomainData

defaultDomainData :: IO DomainData
defaultDomainData = do
  reqQ <- newTQueueIO
  resQ <- newTQueueIO
  cmdQ <- newTQueueIO
  return DomainData {
           _logDirDomainData        = Nothing
         , _logLevelDomainData      = LevelDebug
         , _requestQueueDomainData  = reqQ
         , _responseQueueDomainData = resQ
         , _commandQueueDomainData  = cmdQ
         }

-- |
--
type ErrorData = String
type DomainContext a = DomainData -> IO a



