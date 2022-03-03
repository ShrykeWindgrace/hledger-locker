module HLocker.Machinery where
import           Control.Selective
import           Data.List.NonEmpty


selectFirst :: Selective m => NonEmpty (m (Either (NonEmpty a) b)) -> m (Either (NonEmpty a) b)
selectFirst = foldr1 orElse

failLEV :: a -> Either (NonEmpty a) b
failLEV a = Left $ a :| []
