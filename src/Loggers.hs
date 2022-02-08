{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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

type Loggable t m = (MonadTrans t, MonadReader (Loggers m) (t m), Monad m)

logDebug :: Loggable t m => Text -> t m ()
logDebug t = runLoggers (M Debug t)
logInfo :: Loggable t m => Text -> t m ()
logInfo t = runLoggers (M Info t)
logError :: Loggable t m => Text -> t m ()
logError t = runLoggers (M Error t)
logNone :: Loggable t m => Text -> t m ()
logNone t = runLoggers (M None t)

runLoggers :: Loggable t m => M -> t m ()
runLoggers m@(M Error _) = do
    LogAction act <- asks err
    lift $ act m
runLoggers m = do
    LogAction act <- asks out
    lift $ act m