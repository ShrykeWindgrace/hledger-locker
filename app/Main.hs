{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where
import           Assertions                 (logFailedAssertion, recoverJournal,
                                             runAssertions)
import           Control.Monad.Error.Class  (MonadError (catchError))
import           Control.Monad.Except       (ExceptT, runExceptT)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Selective          (Selective)
import           Data.Foldable              (traverse_)
import qualified Data.Text                  as Text
import           FileWorks                  (makeJournalPath, makeLockerPath)
import           Loggers                    (Logger, logDebug, logError,
                                             logNone, makeLoggers)
import           Options.Applicative
import           ParserWorks                (parseLockersToday)
import           Revision                   (gitVersion)
import           System.Exit                (ExitCode (ExitFailure), exitWith)
import           Types                      (Fails (..), prettyIOFail,
                                             showLockerError)

main :: IO ()
main = do
    CliOptions {..} <- execParser cliOptions
    if showVersion then
        putStrLn gitVersion
    else do
        runApp (makeLoggers verbose) $ global pathToJournal pathToLocker


data CliOptions = CliOptions {
        showVersion   :: Bool,
        pathToJournal :: Maybe FilePath,
        pathToLocker  :: Maybe FilePath,
        verbose       :: Bool
    }

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions <$>
    switch (short 'v' <> long "version" <> help "Print version and exit") <*>
    optional (strOption (short 'f' <> long "journal-file" <> help "path to JOURNAL_FILE" <> metavar "JOURNAL_FILE")) <*>
    optional (strOption (long "locker-file" <> help "path to LOCKER_FILE" <> metavar "LOCKER_FILE")) <*>
    switch (long "debug" <> hidden)

cliOptions :: ParserInfo CliOptions
cliOptions = info (helper <*> cliOptionsParser) (fullDesc <> progDesc "Close/Open account assertions for hledger journal files" <> header "hledger-locker")

newtype App a = App {app :: ReaderT (Logger App) (ExceptT Fails IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadError Fails, MonadReader (Logger App), MonadIO, Selective)



runApp :: Logger App -> App ()  -> IO ()
runApp logs (App app) = do
    z <- runExceptT $ runReaderT app logs
    case z of
        Right () -> pure ()
        Left _ -> putStrLn "Impossible happened: all fails should have been logged by now"


global :: Maybe FilePath -> Maybe FilePath -> App ()
global mjp mlp = do
    jp <- makeJournalPath mjp
    lp <- makeLockerPath mlp
    (errs, ls) <- parseLockersToday lp
    traverse_ logDebug $ fmap showLockerError errs
    j <- recoverJournal jp
    case runAssertions j ls of
        [] -> logDebug "Ok"
        z -> traverse_ logFailedAssertion z >> liftIO (exitWith (ExitFailure 4))
    `catchError` handler

handler :: Fails -> App ()
handler (LParsing e) = logError "Locker parsing failed:" >> logNone (Text.pack e) >> liftIO (exitWith (ExitFailure 3))
handler (JParsing e) = logError "Journal parsing failed:" >> logNone (Text.pack e) >> liftIO (exitWith (ExitFailure 2))
handler (Fs tag iofs) = logError "IO error:" >> logNone (Text.pack $ prettyIOFail tag iofs) >> liftIO (exitWith (ExitFailure 1))
