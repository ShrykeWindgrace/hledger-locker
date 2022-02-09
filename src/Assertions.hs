{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
module Assertions where
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Hledger.Data.Types as HDT
import Types (Locker (Locker, verb, date, acc), Verb (Close, Open), Fails (JParsing), Fails(Asserts))
import Data.Time.Calendar (Day)
import Control.Monad.Except (ExceptT, runExceptT)
import qualified Data.Text.IO as Text
import Loggers (Loggable, logDebug)
import Control.Monad.Error.Class ( MonadError(throwError) )
import Control.Monad.IO.Class ( MonadIO(..) )
import System.Exit (ExitCode (ExitSuccess))
import Data.Maybe (fromMaybe)
import Hledger.Read.JournalReader (parseAndFinaliseJournal, journalp)
import Hledger.Read.Common (definputopts)

data Result = Ok | NotOk (NonEmpty HDT.Transaction)

runAssertion :: HDT.Journal -> Locker Day -> Result
runAssertion (HDT.jtxns -> txns) l = let
    errings = Prelude.filter (compTxn l) txns
    in
        case errings of
            [] -> Ok
            (h:t) -> NotOk $ h :| t

-- | True if candidate violates verb condition
comparator :: Verb -> Day -> Day -> Bool
comparator Close mark candidate = candidate > mark
comparator Open mark candidate = candidate < mark

-- | True if this txn deals with account name and violates the verb
compTxn :: Locker Day -> HDT.Transaction -> Bool
compTxn l t = any (compPosting l) $ fmap (HDT.tdate t,) (HDT.tpostings t)

compPosting :: Locker Day -> (Day, HDT.Posting) -> Bool
compPosting Locker{..} (d, p) = let day = fromMaybe d (HDT.pdate p) in
    HDT.paccount p == acc && comparator verb date day

recoverJournal :: FilePath -> ExceptT String IO HDT.Journal
recoverJournal fp =
    liftIO (Text.readFile fp) >>= parseAndFinaliseJournal journalp definputopts fp


runner :: (Loggable m, MonadError Fails m, MonadIO m) => Locker Day -> FilePath -> m ExitCode
runner l fp = do
    jrc <- liftIO $ runExceptT $ recoverJournal fp
    case jrc of
      Left err -> do
          throwError (JParsing err)
      Right journal -> do
          let ra = runAssertion journal l
          case ra of
              Ok -> 
                  ExitSuccess <$ logDebug "Ok"
              NotOk ne -> throwError $ Asserts $ pure (l, ne)
                  
                  
