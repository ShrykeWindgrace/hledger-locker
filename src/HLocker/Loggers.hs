{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module HLocker.Loggers (Logger, Loggable, logDebug, logError, logNone, logInfo, makeLoggers, runLoggers) where
import           Colog.Core                 (LogAction (LogAction), cfilter)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Data.Maybe                 (isJust)
import           Data.Text                  (Text, pack)
import qualified Data.Text.IO               as TIO
import           System.Console.ANSI        (Color (..), ColorIntensity (Vivid),
                                             ConsoleLayer (Foreground),
                                             SGR (..), hSupportsANSI, setSGRCode)
import           System.Environment         (lookupEnv)
import           System.IO                  (stderr, stdout)

data Sev = None | Debug | Info | Error

data Msg = Msg Sev Text

isDebug :: Msg -> Bool
isDebug (Msg Debug _) = True
isDebug _             = False

-- | Diagnostics and the violation report go to stderr; plain info/debug to stdout.
toStderr :: Msg -> Bool
toStderr (Msg Error _) = True
toStderr (Msg None _)  = True
toStderr _             = False

fmt :: Bool -> Msg -> Text
fmt colour (Msg s t) =  showSeverity colour s <> t

showSeverity :: Bool -> Sev -> Text
showSeverity colour = \case
    None  -> mempty
    Debug -> colourize colour green "[Debug] "
    Info  -> colourize colour blue  "[Info ] "
    Error -> colourize colour red   "[Error] "

colourize :: Bool -> Text -> Text -> Text
colourize True  c t = c <> t <> reset
colourize False _ t = t

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

makeLoggers :: MonadIO m => Bool -> IO (Logger m)
makeLoggers useDebug = do
    noColour <- isJust <$> lookupEnv "NO_COLOR"
    outColour <- if noColour then pure False else hSupportsANSI stdout
    errColour <- if noColour then pure False else hSupportsANSI stderr
    let render msg =
            let (h, colour) = if toStderr msg then (stderr, errColour) else (stdout, outColour)
            in TIO.hPutStrLn h (fmt colour msg)
    pure $ filt useDebug $ LogAction (liftIO . render)
  where
    filt b = if b then id else dropDebug
