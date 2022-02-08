{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module FileWorks where
import Loggers ( logDebug, Loggable )
import Control.Monad.Reader.Class ()
import Control.Selective (Selective)
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.Trans.Class ()
import Control.Monad.Error.Class ( MonadError(throwError) )
import Types ( Fails(Fs), IOFail(..) )
import System.Environment (lookupEnv)
import System.Directory (getHomeDirectory, doesFileExist)
import Machinery (selectFirst, failLEV)
import Data.Bool (bool)
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty ((:|)))
import System.FilePath ((</>))


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

selectFilePath :: (Loggable t m, MonadIO (t m), Selective (t m), MonadError Fails (t m)) => FileConf a -> t m FilePath
selectFilePath FileConf{..} = let explicit = liftIO $ maybe (pure $ failLEV FileNotProvided) valFP providedFP in do
    viaEnv_ <- liftIO $ lookupEnv envVar
    let viaEnvFP = case viaEnv_ of
                            Just viaEnv ->  logDebug "check via env" *> liftIO (valFP viaEnv)
                            Nothing ->  failLEV (EnvVarNotSet envVar) <$ logDebug "env var not set"
    hd <- liftIO getHomeDirectory
    let viaHomeDirFP = liftIO $ valFP (hd </> defaultName)
    z <- selectFirst $ explicit  :| [ viaEnvFP,  logDebug "checked homedir" *> viaHomeDirFP]
    case z of
        Left ne -> throwError (Fs tag ne)
        Right s -> pure s

valFP :: FilePath -> IO (Either (NonEmpty IOFail ) FilePath)
valFP fp = doesFileExist fp <&> bool (failLEV $ FileNotFound fp) (Right fp)

makeJournalPath ::(Loggable t m, MonadIO (t m), Selective (t m), MonadError Fails (t m)) => Maybe FilePath -> t m FilePath
makeJournalPath = selectFilePath . defaultJournalConfig

makeLockerPath ::(Loggable t m, MonadIO (t m), Selective (t m), MonadError Fails (t m)) => Maybe FilePath -> t m FilePath
makeLockerPath = selectFilePath . defaultLockerConfig