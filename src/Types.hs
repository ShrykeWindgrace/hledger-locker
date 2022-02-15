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


data LockerError = LockerError {source :: Text, message :: String} | EmptyFile FilePath deriving stock (Eq, Show)

showLockerError :: LockerError -> Text
showLockerError (EmptyFile fp) = Text.pack $ "File is empty: " <> fp
showLockerError (LockerError s m) = "Parsing error at \"" <> s <> "\" with message " <> Text.pack m


data Fails = Fs String (NonEmpty IOFail) | JParsing String | LParsing String



data Verb = Close | Open deriving stock (Eq, Show)


data Locker a = Locker {
    verb :: Verb,
    date :: a,
    acc  :: HDT.AccountName
} deriving stock (Show, Functor)

instance Eq (Locker HDT.SmartDate) where
    Locker v d a == Locker v2 d2 a2 =
        v == v2 && a == a2 && show d == show d2

instance Eq (Locker Day) where
    Locker v d a == Locker v2 d2 a2 =
        v == v2 && a == a2 && d == d2

type LockerAbs = Locker Day

showLocker :: Locker Day -> Text
showLocker Locker {..} = Text.unwords [Text.pack $ show verb, Text.pack $ show date, acc]
