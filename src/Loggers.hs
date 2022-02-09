{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Loggers where
import Data.Text (Text)
import Colourista
import Colog.Core (LogAction (LogAction))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class

data Sev = None | Debug | Info | Error deriving stock (Eq, Ord)

data M = M Sev Text

fmt :: Maybe Sev -> Text -> Text
fmt ms t = foldMap showSeverity ms <> t

showMsg :: M -> Text
showMsg (M s t) = fmt (Just s) t

showSeverity :: Sev -> Text
showSeverity = \case
    None    -> mempty
    Debug   -> green  <> "[Debug] " <> reset
    Info    -> blue <>  "[Info ] " <> reset
    Error   -> red  <>  "[Error] " <> reset

data Loggers m = Loggers {
    out :: LogAction m M,
    err :: LogAction m M
}

type Loggable m = (MonadReader (Loggers m) m, Monad m)

logDebug :: Loggable m => Text -> m ()
logDebug t = runLoggers (M Debug t)
logInfo :: Loggable m => Text -> m ()
logInfo t = runLoggers (M Info t)
logError :: Loggable m => Text -> m ()
logError t = runLoggers (M Error t)
logNone :: Loggable m => Text -> m ()
logNone t = runLoggers (M None t)

runLoggers :: Loggable m => M -> m ()
runLoggers m@(M Error _) = do
    LogAction act <- asks err
    act m
runLoggers m = do
    LogAction act <- asks out
    act m
