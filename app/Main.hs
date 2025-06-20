{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Control.Monad              (unless, when)
import           Control.Monad.Error.Class  (MonadError (catchError))
import           Control.Monad.Except       (ExceptT, runExceptT)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Reader       (ReaderT (runReaderT))
import           Control.Monad.Reader.Class (MonadReader)
import           Control.Selective          (Selective (select), selectM)
import           Data.Char                  (toLower)
import           Data.Foldable              (traverse_)
import qualified Data.Text                  as Text
import           HLocker                    (Fails (..), Logger, appVersion,
                                             getLockers, gitVersion, logDebug,
                                             logError, logFailedAssertion, logInfo,
                                             logNone, makeJournalPath,
                                             makeLockerPath, makeLoggers,
                                             mkInputOptions, prettyIOFail,
                                             recoverJournal, runAssertions,
                                             runParseDate, showLockerError)
import           Options.Applicative        (Parser, ParserInfo, command,
                                             execParser, fullDesc, header, help,
                                             helper, hidden, hsubparser, info,
                                             infoOption, long, metavar,
                                             optional, progDesc, short,
                                             showDefault, strOption, switch,
                                             (<|>))
import           System.Directory           (doesFileExist)
import           System.Exit                (ExitCode (ExitFailure), exitWith, exitSuccess)
import System.IO


main :: IO ()
main = do
    CliOptions {..} <- execParser cliOptions
    case com of
        Check {..} ->
            runApp (makeLoggers $ verbose commonOptions) $ global commonOptions pathToJournal $ pathToLocker commonOptions
        Wizard kg -> runApp (makeLoggers $ verbose commonOptions) $ wizard kg $ pathToLocker commonOptions


data CliOptions = CliOptions {
        commonOptions :: CommonOptions,
        com           :: CommandChoice
    }

data CommonOptions = CommonOptions {
        pathToLocker :: Maybe FilePath,
        verbose      :: Bool,
        autoTxns     :: Bool,
        forecastTxns :: Bool
    } deriving stock Show

commonOptionsParser :: Parser CommonOptions
commonOptionsParser = CommonOptions <$>
    optional (strOption (short 'l' <> long "locker-file" <> help "path to LOCKER_FILE" <> metavar "LOCKER_FILE")) <*>
    switch (long "debug" <> hidden) <*>
    switch (long "auto" <> help "enable automatic transactions" <> showDefault) <*>
    switch (long "forecast" <> help "enable forecast transactions for this year" <> showDefault)

data CommandChoice = Wizard Bool | Check {pathToJournal :: Maybe FilePath}

checkParser :: Parser CommandChoice
checkParser  = Check <$> optional (strOption (short 'f' <> long "journal-file" <> help "path to LEDGER_FILE" <> metavar "LEDGER_FILE"))

wizardParser :: Parser CommandChoice
wizardParser = Wizard <$> switch (long "keep-going" <> short 'k' <> showDefault <> help "keep going after a successful addition of a lock")

commandChoiceParser :: Parser CommandChoice
commandChoiceParser = hsubparser (
    command "wizard" (info wizardParser (progDesc "Add new assertions") ) <>
    command "check" ( info checkParser (progDesc "Run existing assertions (default)"))
    ) <|> checkParser

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions <$> commonOptionsParser <*> commandChoiceParser

cliOptions :: ParserInfo CliOptions
cliOptions = info (versioner <*> longVersioner <*> helper <*> cliOptionsParser) (fullDesc <> progDesc "Close/Open account assertions for hledger journal files" <> header "hledger-locker")

versioner :: Parser (a -> a)
versioner = infoOption appVersion $ mconcat [short 'v', long "version", help "Print version and exit"]

longVersioner :: Parser (a -> a)
longVersioner = infoOption gitVersion $ mconcat [long "long-version", help "Print long version and exit"]

newtype App a = App {app :: ReaderT (Logger App) (ExceptT Fails IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadError Fails, MonadReader (Logger App), MonadIO)

instance Selective App where
    select :: App (Either a b) -> App (a -> b) -> App b
    select = selectM


runApp :: Logger App -> App () -> IO ()
runApp logs (App app) = do
    z <- runExceptT $ runReaderT app logs
    case z of
        Right () -> pure ()
        Left _ -> putStrLn "Impossible happened: all fails should have been logged by now"


global :: CommonOptions -> Maybe FilePath -> Maybe FilePath -> App ()
global CommonOptions{autoTxns, forecastTxns} mjp mlp = do
    jp <- makeJournalPath mjp
    lp <- makeLockerPath mlp
    (errs, ls) <- getLockers lp
    traverse_ (logDebug . showLockerError) errs
    iopts <- liftIO $ mkInputOptions autoTxns forecastTxns
    j <- recoverJournal jp iopts
    case runAssertions j ls of
        [] -> logDebug "Ok"
        z -> traverse_ logFailedAssertion z >> liftIO (exitWith (ExitFailure 4))
    `catchError` handler

handler :: Fails -> App ()
handler (LParsing e) = logError "Locker parsing failed:" >> logNone (Text.pack e) >> liftIO (exitWith (ExitFailure 3))
handler (JParsing e) = logError "Journal parsing failed:" >> logNone (Text.pack e) >> liftIO (exitWith (ExitFailure 2))
handler (Fs tag iofs) = logError "IO error:" >> logNone (Text.pack $ prettyIOFail tag iofs) >> liftIO (exitWith (ExitFailure 1))


wizard :: Bool -> Maybe FilePath -> App ()
wizard keepGoing mlp = do
    -- create file if it does not exist
    case mlp of
        Nothing -> pure ()
        Just p -> liftIO $ do
            ex <- doesFileExist p
            unless ex $ appendFile p "; created by hlocker\n"


    lp <- makeLockerPath mlp

    liftIO $ putStr "close/open/quit? (default is 'close')\n> " *> hFlush stdout
    clop <- liftIO getLine
    let act = selectAction clop
    if act == "quit" then
        liftIO exitSuccess
    else do
        liftIO $ putStr "when? (default is today)\n> " *> hFlush stdout
        dte <- liftIO getLine
        liftIO (runParseDate dte) >>= \case
            Left err -> logError err
            Right d -> (liftIO $ do
                putStr "Account name?\n> " *> hFlush stdout
                s <- getLine
                appendFile lp $ unwords [act, show d, s <> "\n"]) >> logInfo (Text.pack $ "updated file '" <> lp <> "'")
        when keepGoing $ wizard keepGoing mlp

    `catchError` handler

selectAction :: String -> String
selectAction "" = "close"
selectAction "quit" = "quit"
selectAction s
    | (toLower <$> s) == "close" = "close"
    | otherwise = "open "