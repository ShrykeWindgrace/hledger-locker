{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}
module Assertions where
import           Control.Monad.Error.Class  (MonadError (throwError))
import           Control.Monad.Except       (runExceptT)
import           Control.Monad.IO.Class     (MonadIO (..))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as Text
import           Data.Time.Calendar         (Day)
import qualified Hledger.Data.Transaction   as HDT
import qualified Hledger.Data.Types         as HDT
import           Hledger.Read.Common        (definputopts)
import           Hledger.Read.JournalReader (journalp, parseAndFinaliseJournal)
import           Loggers                    (Loggable, logError, logNone)
import           Types                      (Fails (JParsing),
                                             Locker (Locker, acc, date, verb),
                                             Verb (Close, Open), showLocker)

type Result = (Locker, HDT.Transaction)

runAssertion :: HDT.Journal -> Locker -> [Result]
runAssertion (HDT.jtxns -> txns) l = (l,) <$> Prelude.filter (compTxn l) txns

runAssertions :: HDT.Journal -> [Locker] -> [Result]
runAssertions j ls = runAssertion j =<< ls

-- | True if candidate violates verb condition
comparator :: Verb -> Day -> Day -> Bool
comparator Close mark candidate = candidate > mark
comparator Open mark candidate  = candidate < mark

-- | True if this txn deals with account name and violates the verb
compTxn :: Locker -> HDT.Transaction -> Bool
compTxn l t = any (compPosting l) $ fmap (HDT.tdate t,) (HDT.tpostings t)

compPosting :: Locker -> (Day, HDT.Posting) -> Bool
compPosting Locker{..} (d, p) = let day = fromMaybe d (HDT.pdate p) in
    HDT.paccount p == acc && comparator verb date day

recoverJournal :: (Loggable m, MonadError Fails m, MonadIO m) => FilePath -> m HDT.Journal
recoverJournal fp = do
    c <- liftIO $ Text.readFile fp
    j <-  liftIO $ runExceptT $ parseAndFinaliseJournal journalp definputopts fp c
    case j of
      Left err      -> throwError (JParsing err)
      Right journal -> pure journal


logFailedAssertion :: Loggable m => Result -> m ()
logFailedAssertion (l, txn) = do
    logError $ showLocker l
    logNone $ HDT.showTransaction txn
