{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE ViewPatterns     #-}
module HLocker.Assertions where
import           Control.Monad.Error.Class      (MonadError (throwError))
import           Control.Monad.Except           (runExceptT)
import           Control.Monad.IO.Class         (MonadIO (..))
import           Data.Maybe                     (fromMaybe)
import qualified Data.Text.IO                   as Text
import           Data.Time
import           Data.Time.Calendar.OrdinalDate (toOrdinalDate)
import qualified Hledger.Data.Transaction       as HDT
import qualified Hledger.Data.Types             as HDT
import           Hledger.Read.Common            (InputOpts (auto_, forecast_),
                                                 definputopts)
import           Hledger.Read.JournalReader     (journalp,
                                                 parseAndFinaliseJournal)
import           HLocker.Loggers                (Loggable, logError, logNone)
import           HLocker.Types                  (Fails (JParsing),
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

recoverJournal :: (Loggable m, MonadError Fails m, MonadIO m) => FilePath -> InputOpts -> m HDT.Journal
recoverJournal fp opts = do
    c <- liftIO $ Text.readFile fp
    j <-  liftIO $ runExceptT $ parseAndFinaliseJournal (journalp opts) opts fp c
    case j of
      Left err      -> throwError (JParsing err)
      Right journal -> pure journal


logFailedAssertion :: Loggable m => Result -> m ()
logFailedAssertion (l, txn) = do
    logError $ showLocker l
    logNone $ HDT.showTransaction txn

mkInputOptions :: Bool -> Bool -> IO InputOpts
mkInputOptions useAuto useForecast = do
    today <- localDay . zonedTimeToLocalTime <$> getZonedTime
    let year = fst $ toOrdinalDate today
    pure $ definputopts {
        auto_ = useAuto,
        forecast_ = if useForecast
            then
                Just $ HDT.DateSpan (Just $ HDT.Exact $ fromGregorian year 1 1) (Just $ HDT.Exact today)
            else
                Nothing
        }
