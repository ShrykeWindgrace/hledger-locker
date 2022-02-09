{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import           Options.Applicative
import Control.Monad.IO.Class
import Loggers
import Control.Monad.Reader.Class
import Control.Monad.Error.Class
import Control.Monad.Reader (ReaderT)
import Control.Monad.Except (ExceptT)
import Types (Fails)

main :: IO ()
main = pure ()


data CliOptions = CliOptions {
        showVersion :: Bool,
        pathToJournal :: Maybe FilePath,
        pathToLocker :: Maybe FilePath,
        verbose :: Bool
    }

cliOptionsParser :: Parser CliOptions
cliOptionsParser = CliOptions <$>
    switch (short 'v' <> long "version" <> help "Print version and exit") <*>
    optional (strOption (short 'f' <> long "journal-file" <> help "path to JOURNAL_FILE" <> metavar "JOURNAL_FILE")) <*>
    optional (strOption (long "locker-file" <> help "path to LOCKER_FILE" <> metavar "LOCKER_FILE")) <*>
    switch (long "debug" <> hidden)

newtype App a = App {app :: ReaderT (Loggers (ExceptT Fails IO)) (ExceptT Fails IO) a}
    deriving newtype (Functor, Applicative, Monad, MonadError Fails, MonadReader (Loggers (ExceptT Fails IO)), MonadIO)

runApp :: App () -> Loggers IO -> IO ()
runApp = error "TODO"
