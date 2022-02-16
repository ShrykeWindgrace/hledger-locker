import           Test.Hspec            (describe, hspec, it)
import           Test.Hspec.Megaparsec (shouldFailOn, shouldParse,
                                        shouldSucceedOn)

import           Data.Time.Calendar    (fromGregorian)
import           ParserWorks           (parseLocker)
import           Text.Megaparsec       (runParser)
import           Types                 (Locker (Locker), Verb (Close))

main :: IO ()
main = hspec $ do
    describe "parse locker lines" $ do
        let parser = runParser parseLocker ""
        it "manual testing" $ do
            parser `shouldFailOn` ""
            parser `shouldFailOn` " "
            parser `shouldFailOn` ";close 2021-01-02 accountname"
            parser `shouldFailOn` "#close 2021-01-02 accountname"
            parser `shouldFailOn` "abracadabra"
            parser `shouldFailOn` "abracadabra"
            parser `shouldSucceedOn` "close 2021-01-02 accountname"
            parser `shouldSucceedOn` "close 2021-01-02 accountname "
            parser `shouldSucceedOn` "Close 2021-1-2 accountname "
            parser "close 2021-01-02 accountname" `shouldParse` Locker Close (fromGregorian 2021 1 2) "accountname"
        it "do not accept partial dates" $ do
            parser `shouldFailOn` "close 01/02 account"
            parser `shouldFailOn` "close 02 account"
        it "do not accept smart dates" $
            parser `shouldFailOn` "close yesterday account"
        it "do not accept partial years" $
            parser `shouldFailOn` "close 22-01-02 account"


