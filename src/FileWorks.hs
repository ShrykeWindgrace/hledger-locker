{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}

module FileWorks (makeJournalPath, makeLockerPath) where
import           Control.Monad.Error.Class  (MonadError (throwError))
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader.Class ()
import           Control.Monad.Trans.Class  ()
import           Control.Selective          (Selective)
import           Data.Bool                  (bool)
import           Data.Functor               ((<&>))
import           Data.List.NonEmpty         (NonEmpty ((:|)))
import qualified Data.Text                  as Text
import           Loggers                    (Loggable, logDebug)
import           Machinery                  (failLEV, selectFirst)
import           System.Directory           (doesFileExist, getHomeDirectory)
import           System.Environment         (lookupEnv)
import           System.FilePath            ((</>))
import           Types                      (Fails (Fs), IOFail (..))

data FileConf a = FileConf {
    envVar      :: String,
    defaultName :: FilePath,
    tag         :: String,
    providedFP  :: Maybe FilePath
}



data JournalTag
data LockerTag

defaultJournalConfig :: Maybe FilePath -> FileConf JournalTag
defaultJournalConfig = FileConf "LEDGER_FILE" ".hledger.journal" "journal"

defaultLockerConfig :: Maybe FilePath -> FileConf LockerTag
defaultLockerConfig = FileConf "HLEDGER_LOCKER_FILE" ".hledger.locker" "locker"

selectFilePath :: (Loggable m, MonadIO m, Selective m, MonadError Fails m) => FileConf a -> m FilePath
selectFilePath FileConf{..} = let explicit = liftIO $ maybe (pure $ failLEV FileNotProvided) valFP providedFP in do
    viaEnv_ <- liftIO $ lookupEnv envVar
    let viaEnvFP = case viaEnv_ of
                            Just viaEnv -> do
                                logDebug (Text.pack $ tag <> ": check via env: " <> envVar) *> liftIO (valFP viaEnv)
                            Nothing ->  failLEV (EnvVarNotSet envVar) <$ logDebug (Text.pack $ tag <> ": env var not set: " <> envVar)
    hd <- liftIO getHomeDirectory
    let viaHomeDirFP = liftIO $ valFP (hd </> defaultName)
    z <- selectFirst $ explicit  :| [ viaEnvFP,  logDebug (Text.pack $ tag <> ": checked homedir") *> viaHomeDirFP]
    case z of
        Left ne -> throwError (Fs tag ne)
        Right s -> s <$ logDebug (Text.pack $ tag <> ": selected: " <> s)

valFP :: FilePath -> IO (Either (NonEmpty IOFail ) FilePath)
valFP fp = doesFileExist fp <&> bool (failLEV $ FileNotFound fp) (Right fp)

makeJournalPath :: (Loggable m, MonadIO m, Selective m, MonadError Fails m) => Maybe FilePath -> m FilePath
makeJournalPath = selectFilePath . defaultJournalConfig

makeLockerPath :: (Loggable m, MonadIO m, Selective m, MonadError Fails m) => Maybe FilePath -> m FilePath
makeLockerPath = selectFilePath . defaultLockerConfig
