import Test.Hspec ( it, describe, hspec )
import Test.Hspec.Megaparsec
    ( shouldParse, shouldFailOn, shouldSucceedOn )

import Hledger.Data.Types ( SmartDate(SmartAssumeStart) )
import Data.Time.Calendar ( fromGregorian )
import Types
import ParserWorks
import Text.Megaparsec ( runParser )

main :: IO ()
main = hspec $ do    
    describe "parse locker lines" $ do
        it "naive" $ do
            let parser = runParser parseLocker ""
            parser `shouldFailOn` ""
            parser `shouldFailOn` " "
            parser `shouldFailOn` ";close 2021-01-02 accountname"
            parser `shouldFailOn` "#close 2021-01-02 accountname"
            parser `shouldSucceedOn` "close 2021-01-02 accountname"
            parser `shouldSucceedOn` "close 2021-01-02 accountname "

            parser "close 2021-01-02 accountname" `shouldParse` Locker Close (SmartAssumeStart 2021 (Just (1, Just 2))) "accountname"

        it "based on nulldate" $ do
            let parser = runParser parseLockerAbsolute ""
            parser "close 2021-01-02 accountname" `shouldParse` Locker Close (fromGregorian 2021 1 2) "accountname"
            parser "close 2021-01-02 accountname " `shouldParse` Locker Close (fromGregorian 2021 1 2) "accountname"
