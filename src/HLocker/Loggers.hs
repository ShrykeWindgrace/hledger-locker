{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
module HLocker.Loggers (Logger, Loggable, logDebug, logError, logNone, logInfo, makeLoggers, runLoggers) where
import           Colog.Core                 (LogAction (LogAction), cfilter)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Functor.Contravariant ((>$<))
import           Data.Text                  (Text, pack)
import qualified Data.Text.IO               as TIO
import           System.Console.ANSI        (Color (..), ColorIntensity (Vivid),
                                             ConsoleLayer (Foreground),
                                             SGR (..), setSGRCode)

data Sev = None | Debug | Info | Error

data Msg = Msg Sev Text

isDebug :: Msg -> Bool
isDebug (Msg Debug _) = True
isDebug _             = False

fmt :: Bool -> Msg -> Text
fmt useColor (Msg s t) = showSeverity useColor s <> t

showSeverity :: Bool -> Sev -> Text
showSeverity _ None          = mempty
showSeverity False Debug     = "[Debug] "
showSeverity False Info      = "[Info ] "
showSeverity False Error     = "[Error] "
showSeverity True  Debug     = green <> "[Debug] " <> reset
showSeverity True  Info      = blue  <> "[Info ] " <> reset
showSeverity True  Error     = red   <> "[Error] " <> reset

red :: Text
red = pack $ setSGRCode [SetColor Foreground Vivid Red]

green :: Text
green = pack $ setSGRCode [SetColor Foreground Vivid Green]

blue :: Text
blue = pack $ setSGRCode [SetColor Foreground Vivid Blue]

reset :: Text
reset = pack $ setSGRCode [Reset]

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
dropDebug = cfilter (not . isDebug)

makeLoggers :: MonadIO m => Bool -> Bool -> Logger m
makeLoggers useDebug useColor = filt useDebug $ fmt useColor >$< LogAction (liftIO . TIO.putStrLn) where
    filt b = if b then id else dropDebug
