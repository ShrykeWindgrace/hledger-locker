{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Loggers where
import           Colog.Core                 (LogAction (LogAction))
import           Colourista                 (blue, green, red, reset)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO

data Sev = None | Debug | Info | Error deriving stock (Eq, Ord)

data Msg = Msg Sev Text

fmt :: Msg -> Text
fmt (Msg s t) =  showSeverity s <> t

showSeverity :: Sev -> Text
showSeverity = \case
    None  -> mempty
    Debug -> green  <> "[Debug] " <> reset
    Info  -> blue <>  "[Info ] " <> reset
    Error -> red  <>  "[Error] " <> reset

type Logger m = LogAction m Msg

type Loggable m = (MonadReader (Logger m) m, Monad m)

logDebug :: Loggable m => Text -> m ()
logDebug = runLoggers . Msg Debug
logInfo :: Loggable m => Text -> m ()
logInfo = runLoggers . Msg Info
logError :: Loggable m => Text -> m ()
logError = runLoggers . Msg Error
logNone :: Loggable m => Text -> m ()
logNone = runLoggers . Msg None

runLoggers :: Loggable m => Msg -> m ()
runLoggers m = do
    LogAction act <- ask
    act m

dropDebug :: Applicative m => Logger m -> Logger m
dropDebug (LogAction act) = LogAction $ \case
    Msg Debug _ -> pure ()
    msg         -> act msg

makeLoggers :: MonadIO m => Bool -> Logger m
makeLoggers useDebug = filt useDebug $ fmt >$< LogAction (liftIO . TIO.putStrLn) where
    filt b = if b then id else dropDebug
