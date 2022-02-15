{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}
module ParserWorks where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Bifunctor         (Bifunctor (bimap))
import           Data.Either            (partitionEithers)
import           Data.Maybe             (catMaybes)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.IO           as TIO
import           Data.Time.Calendar     (Day)
import qualified Hledger.Data.Dates     as HDD
import qualified Hledger.Data.Types     as HDT
import qualified Hledger.Read.Common    as HRC
import qualified Hledger.Utils.Parse    as HUP
import           Text.Megaparsec        (Stream (Token), choice,
                                         errorBundlePretty, runParser)

import           Text.Megaparsec.Char   (hspace1, string')
import           Types



parseVerb :: HUP.TextParser m Verb
parseVerb = choice [
        Close <$ string' "close",
        Open <$ string' "open"
    ]

commentMarkers :: [Token Text]
commentMarkers = ";#"


parseLocker :: HUP.TextParser m (Locker HDT.SmartDate)
parseLocker = do
    verb <- parseVerb
    hspace1
    date <- HDD.smartdate
    hspace1
    acc  <- HRC.accountnamep
    pure $ Locker {..}

parseLockerAbsolute :: HUP.TextParser m (Locker Day)
parseLockerAbsolute = fmap (HDD.fixSmartDate HDD.nulldate) <$> parseLocker

parseLockerToday :: MonadIO m => HUP.TextParser m (Locker Day)
parseLockerToday = do
    day <- liftIO HDD.getCurrentDay
    l <- parseLocker
    pure $ HDD.fixSmartDate day <$> l

lockerToday :: Locker HDT.SmartDate -> IO (Locker Day)
lockerToday l = (\d -> HDD.fixSmartDate d <$> l) <$> HDD.getCurrentDay



-- FilePath is assumed to exist
parseLockers :: MonadIO m => FilePath -> m ([LockerError], [Locker HDT.SmartDate])
parseLockers fp = do
    ls <- Prelude.filter (not . Text.null) . Text.lines <$> liftIO (TIO.readFile fp)
    pure $ if null ls then ([EmptyFile fp], []) else catMaybes <$> partitionEithers (single <$> ls)
    where
        single :: Text -> Either LockerError (Maybe (Locker HDT.SmartDate))
        single t
            | Text.head t `elem` commentMarkers = Right Nothing
            | otherwise = bimap (LockerError t . errorBundlePretty) Just $ runParser parseLocker fp t


parseLockersToday :: MonadIO m => FilePath -> m ([LockerError], [Locker Day])
parseLockersToday fp = do
    g <- parseLockers fp
    traverse (traverse (liftIO . lockerToday)) g
