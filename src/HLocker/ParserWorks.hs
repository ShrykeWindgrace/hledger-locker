{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module HLocker.ParserWorks (getLockers, parseLocker) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.State.Strict (runStateT)
import           Data.Bifunctor             (Bifunctor (bimap))
import           Data.Either                (partitionEithers)
import           Data.Maybe                 (catMaybes)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.IO               as TIO
import           Data.Time.Calendar         (Day)
import           Hledger.Data.Journal       (nulljournal)
import qualified Hledger.Read.Common        as HRC
import qualified Hledger.Utils.Parse        as HUP
import           HLocker.Types              (Locker (..),
                                             LockerError (EmptyFile, LockerError),
                                             Verb (..))
import           Text.Megaparsec            (Stream (Token), choice,
                                             errorBundlePretty, runParser)
import           Text.Megaparsec.Char       (hspace1, string')



parseVerb :: HUP.TextParser m Verb
parseVerb = choice [
        Close <$ string' "close",
        Open <$ string' "open"
    ]

commentMarkers :: [Token Text]
commentMarkers = ";#"


parseLocker :: HUP.TextParser m Locker
parseLocker = do
    verb <- parseVerb
    hspace1
    date <- parseDate
    hspace1
    acc  <- HRC.accountnamep
    pure $ Locker {..}


-- FilePath is assumed to exist
getLockers :: MonadIO m => FilePath -> m ([LockerError], [Locker])
getLockers fp = do
    ls <- Prelude.filter (not . Text.null) . Text.lines <$> liftIO (TIO.readFile fp)
    pure $ if null ls then ([EmptyFile fp], []) else catMaybes <$> partitionEithers (single <$> ls)
    where
        single :: Text -> Either LockerError (Maybe Locker)
        single t
            | Text.head t `elem` commentMarkers = Right Nothing
            | otherwise = bimap (LockerError t . errorBundlePretty) Just $ runParser parseLocker fp t


parseDate :: HUP.TextParser m Day
parseDate = fst <$> runStateT HRC.datep nulljournal
