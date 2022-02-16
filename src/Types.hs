{-# LANGUAGE DeriveFunctor   #-}
{-# LANGUAGE RecordWildCards #-}
module Types where
import           Data.List.NonEmpty (NonEmpty, toList)
import           Data.Text          (Text)
import qualified Data.Text          as Text
import           Data.Time.Calendar (Day)
import qualified Hledger.Data.Types as HDT

data IOFail = EnvVarNotSet String | FileNotFound FilePath | FileNotProvided deriving stock Show


prettyIOFail :: String -> NonEmpty IOFail -> String
prettyIOFail prefix fs = let indent = replicate (length prefix) ' ' in
    unlines $ prefix : toList ((indent <>) . show <$> fs)


data LockerError = LockerError {source :: Text, message :: String} | EmptyFile FilePath

showLockerError :: LockerError -> Text
showLockerError (EmptyFile fp) = Text.pack $ "File is empty: " <> fp
showLockerError (LockerError s m) = "Parsing error at \"" <> s <> "\" with message " <> Text.pack m


data Fails = Fs String (NonEmpty IOFail) | JParsing String | LParsing String



data Verb = Close | Open deriving stock (Eq, Show)


data Locker = Locker {
    verb :: Verb,
    date :: Day,
    acc  :: HDT.AccountName
} deriving stock (Eq, Show)


showLocker :: Locker -> Text
showLocker Locker {..} = Text.unwords [Text.pack $ show verb, Text.pack $ show date, acc]
