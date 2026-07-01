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
import           Data.Char                  (isSpace, toLower)
import           Data.Foldable              (traverse_)
import           Data.List                  (partition)
import qualified Data.Text                  as Text
import           HLocker                    (Fails (..), Logger,
                                             Verb (Close, Open), appVersion,
                                             getLockers, gitVersion, isEmptyFile,
                                             logDebug, logError,
                                             logFailedAssertion, logInfo, logNone,
                                             makeJournalPath, makeLockerPath,
                                             makeLoggers, mkInputOptions,
                                             prettyIOFail, recoverJournal,
                                             resolveLockerPath, runAssertions,
                                             runParseDate, showLockerError)
import           Options.Applicative        (Parser, ParserInfo, command,
                                             execParser, fullDesc, header, help,
                                             helper, hidden, hsubparser, info,
                                             infoOption, long, metavar,
                                             optional, progDesc, short,
                                             showDefault, strOption, switch,
                                             (<|>))
import           System.Directory           (doesFileExist)
import           System.Exit                (ExitCode (ExitFailure), exitSuccess,
                                             exitWith)
import           System.IO                  (hFlush, hPutStrLn, stderr, stdout)


main :: IO ()
main = do
    CliOptions {..} <- execParser cliOptions
    logs <- makeLoggers $ verbose commonOptions
    case com of
        Check {..} ->
            runApp logs $ global commonOptions pathToJournal $ pathToLocker commonOptions
        Wizard kg -> runApp logs $ wizard kg $ pathToLocker commonOptions


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
        Left _ -> do
            hPutStrLn stderr "Impossible happened: all fails should have been logged by now"
            exitWith (ExitFailure 255)


global :: CommonOptions -> Maybe FilePath -> Maybe FilePath -> App ()
global CommonOptions{autoTxns, forecastTxns} mjp mlp = do
    jp <- makeJournalPath mjp
    lp <- makeLockerPath mlp
    (errs, ls) <- getLockers lp
    let (empties, parseErrs) = partition isEmptyFile errs
    -- An empty locker file is a warning, not a failure: there is nothing to check.
    traverse_ (logInfo . showLockerError) empties
    -- Genuine parse errors are reported, but we still check the lines that parsed.
    traverse_ (logError . showLockerError) parseErrs
    iopts <- liftIO $ mkInputOptions autoTxns forecastTxns
    j <- recoverJournal jp iopts
    let failures = runAssertions j ls
    traverse_ logFailedAssertion failures
    if not (null failures)
        then liftIO $ exitWith (ExitFailure 4)
        else if not (null parseErrs)
            then liftIO $ exitWith (ExitFailure 3)
            else logDebug "Ok"
    `catchError` handler

handler :: Fails -> App ()
handler (JParsing e) = logError "Journal parsing failed:" >> logNone (Text.pack e) >> liftIO (exitWith (ExitFailure 2))
handler (Fs tag iofs) = logError "IO error:" >> logNone (Text.pack $ prettyIOFail tag iofs) >> liftIO (exitWith (ExitFailure 1))


data WizardAction = Quit | Act Verb

selectAction :: String -> Maybe WizardAction
selectAction s = case toLower <$> trim s of
    ""      -> Just (Act Close) -- default
    "close" -> Just (Act Close)
    "open"  -> Just (Act Open)
    "quit"  -> Just Quit
    _       -> Nothing

verbKeyword :: Verb -> String
verbKeyword Close = "close"
verbKeyword Open  = "open"

trim :: String -> String
trim = f . f where f = reverse . dropWhile isSpace

wizard :: Bool -> Maybe FilePath -> App ()
wizard keepGoing mlp = do
    -- Resolve the target path first, creating the file if it does not exist yet,
    -- so the wizard can bootstrap a brand-new locker file at any location.
    lp <- resolveLockerPath mlp
    liftIO $ do
        ex <- doesFileExist lp
        unless ex $ appendFile lp "; created by hlocker\n"

    liftIO $ putStr "close/open/quit? (default is 'close')\n> " *> hFlush stdout
    clop <- liftIO getLine
    case selectAction clop of
        Nothing -> logError "Unrecognised action; expected 'close', 'open', or 'quit'"
        Just Quit -> liftIO exitSuccess
        Just (Act v) -> do
            liftIO $ putStr "when? (default is today)\n> " *> hFlush stdout
            dte <- liftIO getLine
            liftIO (runParseDate dte) >>= \case
                Left err -> logError err
                Right d -> do
                    liftIO $ putStr "Account name?\n> " *> hFlush stdout
                    acc <- trim <$> liftIO getLine
                    if null acc
                        then logError "Empty account name; nothing written"
                        else do
                            liftIO $ appendFile lp $ unwords [verbKeyword v, show d, acc] <> "\n"
                            logInfo (Text.pack $ "updated file '" <> lp <> "'")
            when keepGoing $ wizard keepGoing mlp

    `catchError` handler
