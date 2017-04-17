{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text  (Text)
import qualified Data.Text  as T
import           Test.Hspec

import           Bottles


removeSpaces :: Text -> Text
removeSpaces = T.replace " " ""

main :: IO ()
main = hspec $
  describe "99 bottles" $ do
    it "should display the first verse" $ do
      let firstVerse = "99 bottles of beer on the wall, \
                       \99 bottles of beer. \n\
                       \Take one down and pass it around, \
                       \98 bottles of beer on the wall."
      verse 99 `shouldBe` firstVerse
    it "should display another verse" $ do
      let anotherVerse = "3 bottles of beer on the wall, \
                         \3 bottles of beer. \n\
                         \Take one down and pass it around, \
                         \2 bottles of beer on the wall."
      verse 3 `shouldBe` anotherVerse
    it "should display the third-from-last verse" $ do
      let thirdFromLast = "2 bottles of beer on the wall, \
                          \2 bottles of beer. \n\
                          \Take one down and pass it around, \
                          \1 bottle of beer on the wall."
      verse 2 `shouldBe` thirdFromLast
    it "should display the next-to-last verse" $ do
      let nextToLast = "1 bottle of beer on the wall, \
                       \1 bottle of beer. \n\
                       \Take it down and pass it around, \
                       \No more bottles of beer on the wall."
      verse 1 `shouldBe` nextToLast
    it "should display the last verse" $ do
      let lastVerse = "No more bottles of beer on the wall, \
                      \no more bottles of beer. \n\
                      \Go to the store and buy some more, \
                      \99 bottles of beer on the wall."
      verse 0 `shouldBe` lastVerse
