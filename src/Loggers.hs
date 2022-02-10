{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Loggers where
import Data.Text (Text)
import Colourista ( blue, green, red, reset )
import Colog.Core (LogAction (LogAction))
import Control.Monad.Reader.Class ( MonadReader, asks )
import qualified Data.Text.IO as TIO
import System.IO ( stderr )
import Data.Functor.Contravariant
import Control.Monad.IO.Class

data Sev = None | Debug | Info | Error deriving stock (Eq, Ord)

data Msg = Msg Sev Text

fmt :: Msg -> Text
fmt (Msg s t) =  showSeverity s <> t

showSeverity :: Sev -> Text
showSeverity = \case
    None    -> mempty
    Debug   -> green  <> "[Debug] " <> reset
    Info    -> blue <>  "[Info ] " <> reset
    Error   -> red  <>  "[Error] " <> reset

data Loggers m = Loggers {
    out :: LogAction m Msg,
    err :: LogAction m Msg
}

type Loggable m = (MonadReader (Loggers m) m, Monad m)

logDebug :: Loggable m => Text -> m ()
logDebug = runLoggers . Msg Debug
logInfo :: Loggable m => Text -> m ()
logInfo = runLoggers . Msg Info
logError :: Loggable m => Text -> m ()
logError = runLoggers . Msg Error
logNone :: Loggable m => Text -> m ()
logNone = runLoggers . Msg None

runLoggers :: Loggable m => Msg -> m ()
runLoggers m@(Msg Error _) = do
    LogAction act <- asks err
    act m
runLoggers m@(Msg None _) = do
    LogAction act <- asks err
    act m
runLoggers msg = do
    LogAction act <- asks out
    act msg

dropDebug :: Applicative m => LogAction m Msg -> LogAction m Msg
dropDebug (LogAction act) = LogAction $ \case
    Msg Debug _ -> pure ()
    msg -> act msg

makeLoggers :: MonadIO m => Bool -> Loggers m
makeLoggers useDebug = Loggers {out = filt useDebug $ fmt >$< LogAction (liftIO . TIO.putStrLn), err = filt useDebug $ fmt >$<  LogAction (liftIO . TIO.hPutStrLn stderr)} where
    filt b = if b then id else dropDebug