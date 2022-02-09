{-# LANGUAGE DeriveFunctor #-}
module Types where
import Data.List.NonEmpty (toList, NonEmpty)
import Data.Text (Text)
import qualified Hledger.Data.Types as HDT
import Data.Time.Calendar (Day)

data IOFail = EnvVarNotSet String | FileNotFound FilePath | FileNotProvided deriving stock Show


prettyIOFail :: String -> NonEmpty IOFail -> String
prettyIOFail prefix fs = let indent = replicate (length prefix) ' ' in
    unlines $ prefix : toList ((indent <>) . show <$> fs)


data LockerError = LockerError {source :: Text, message :: String} | EmptyFile deriving stock (Eq, Show)

data Fails = Fs String (NonEmpty IOFail) | JParsing String | LParsing String | Asserts (NonEmpty (Locker Day, NonEmpty HDT.Transaction))



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