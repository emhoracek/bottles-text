{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Text       (Text)
import qualified Data.Text       as T
import           Test.Hspec
import           Test.QuickCheck

import           Bottles

removeSpaces :: Text -> Text
removeSpaces = T.replace " " ""

main :: IO ()
main = hspec $ do
  describe "verse" $ do
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
                       \no more bottles of beer on the wall."
      verse 1 `shouldBe` nextToLast
    it "should display the last verse" $ do
      let lastVerse = "No more bottles of beer on the wall, \
                      \no more bottles of beer. \n\
                      \Go to the store and buy some more, \
                      \99 bottles of beer on the wall."
      verse 0 `shouldBe` lastVerse
  describe "verses" $ do
    it "should display a couple of verses" $ do
      let verses98and97 = "98 bottles of beer on the wall, \
                          \98 bottles of beer. \n\
                          \Take one down and pass it around, \
                          \97 bottles of beer on the wall.\
                          \\n\n\
                          \97 bottles of beer on the wall, \
                          \97 bottles of beer. \n\
                          \Take one down and pass it around, \
                          \96 bottles of beer on the wall."
      verses 98 97 `shouldBe` verses98and97
    it "should display from 2 to 0 beers verses" $ do
      let verses2thru0 = "2 bottles of beer on the wall, \
                         \2 bottles of beer. \n\
                         \Take one down and pass it around, \
                         \1 bottle of beer on the wall.\
                         \\n\n\
                         \1 bottle of beer on the wall, \
                         \1 bottle of beer. \n\
                         \Take it down and pass it around, \
                         \no more bottles of beer on the wall.\
                         \\n\n\
                         \No more bottles of beer on the wall, \
                         \no more bottles of beer. \n\
                         \Go to the store and buy some more, \
                         \99 bottles of beer on the wall."
      verses 2 0 `shouldBe` verses2thru0


    -- Before I read the bit about testing one function against another
    it "should display arbitrary verses" $ property $
      \v1 v2 -> let (Positive n1) = v1
                    (Positive n2) = v2
                    range f = if n1 > n2 then f n1 n2
                                       else f n2 n1 in
                  verse n1 `T.isInfixOf` range verses
                  && verse n2 `T.isInfixOf` range verses


-- what happens when things go wrong?
  describe "through" $ do
    it "should generate a list of integers between two numbers" $
      (5 `through` 1) `shouldBe` [5,4,3,2,1]
    it "should work with both numbers the same" $
      (5 `through` 5) `shouldBe` [5]
    it "should return an empty list for numbers going up" $
      (1 `through` 5) `shouldBe` [1,2,3,4,5]
    it "should work ok with negative numbers" $
      (1 `through` (-3)) `shouldBe` [1,0,-1,-2,-3]
