{-# LANGUAGE TemplateHaskell #-}

module ASA.Domain.Service.Type where

import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader

import qualified ASA.Domain.Model.Type as DM



--------------------------------------------------------------------------------
-- |
--
data StateTransition =
    StartingToRunning
  | RunningToStopping
  deriving (Show, Eq)

-- |
--
data EntryRequestData      = EntryRequestData deriving (Show)
data ExitRequestData       = ExitRequestData  deriving (Show)
data TransitRequestData    = TransitRequestData StateTransition deriving (Show)
-- doActibity
data InitRequestData       = InitRequestData String deriving (Show)
data LaunchRequestData     = LaunchRequestData Int deriving (Show)
data DisconnectRequestData = DisconnectRequestData Int deriving (Show)
data TerminateRequestData  = TerminateRequestData Int  deriving (Show)

-- |
--
data Request r where
  EntryRequest      :: Request EntryRequestData
  ExitRequest       :: Request ExitRequestData
  TransitRequest    :: TransitRequestData -> Request TransitRequestData
  -- doActibity
  InitRequest       :: InitRequestData       -> Request InitRequestData
  LaunchRequest     :: LaunchRequestData     -> Request LaunchRequestData
  DisconnectRequest :: DisconnectRequestData -> Request DisconnectRequestData
  TerminateRequest  :: TerminateRequestData  -> Request TerminateRequestData

deriving instance Show r => Show (Request r)

-- |
--
data RequestW = forall r. RequestW (Request r)




--------------------------------------------------------------------------------
-- Type for Domain Service.
--------------------------------------------------------------------------------
-- |
--
type AppStateContext = StateT AppStateW (ReaderT DM.DomainData (ExceptT DM.ErrorData (LoggingT IO)))
type AppContext = AppStateContext

-- type AppStateContext = StateT AppStateW IO

-- |
--
data StartingStateData = StartingStateData deriving (Show)
data RunningStateData  = RunningStateData  deriving (Show)
data StoppingStateData = StoppingStateData deriving (Show)
data AppState s where
  StartingState :: AppState StartingStateData
  RunningState  :: AppState RunningStateData
  StoppingState :: AppState StoppingStateData

deriving instance Show s => Show (AppState s)

-- |
--
data AppStateW = forall s. (IAppState s, Show s) => AppStateW (AppState s)


-- |
--
class (Show s, Show r) => IStateActivity s r where
  action :: (AppState s) -> (Request r) -> AppStateContext (Maybe StateTransition)
  action s (TransitRequest (TransitRequestData t)) = do
    liftIO $ putStrLn $ show s ++ " " ++ show t ++ " will transit."
    return (Just t)
  action s r = do
    liftIO $ putStrLn $ show s ++ " " ++ show r ++ " not supported. will do nothing."
    return Nothing

-- |
--
class IAppState s where
  actionS  :: AppState s -> RequestW -> AppStateContext (Maybe StateTransition)

-- |
--
class IAppStateW s where
  actionSW  :: s -> RequestW -> AppStateContext (Maybe StateTransition)

instance IAppStateW AppStateW where
  actionSW (AppStateW a) r = actionS a r


