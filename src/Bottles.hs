{-# LANGUAGE OverloadedStrings #-}

module Bottles where

import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as T


tshow :: Show a => a -> Text
tshow = T.pack . show

-- Path is more like:
-- 1) doesn't compile ("Not in scope")
-- 2) Insert blah = undefined
-- 3) uncaught exception: ErrorCall (Prelude.undefined)
-- 4) add function signature and garbage result
-- 5) fix it

-- I have to worry about converting `n` to a string, but sandi doesn't

verse :: Int -> Text
verse 0 =
  "No more bottles of beer on the wall, \
  \no more bottles of beer. \n\
  \Go to the store and buy some more, \
  \99 bottles of beer on the wall."
-- changing the cases to make them the same one little bit at a time is the hardest part.
-- if I keep patternmatching, then get overlapping patterns
-- so first I'm changing this part to be a case statement
verse n =
  case n of
    1 -> tshow n <> " bottle of beer on the wall, " <>
         tshow n <> " bottle of beer. \n\
         \Take it down and pass it around, \
         \no more bottles of beer on the wall."
    _ -> tshow n <> " " <> container n <> " of beer on the wall, " <>
         tshow n <> " " <> container n <> " of beer. \n" <>
         "Take one down and pass it around, " <>
         tshow (n-1) <> " " <> container (n-1) <> " of beer on the wall."

-- can't take temporarily optional parameter
container :: Int -> Text
container 1 = "bottle"
-- it's nice that this is VERY Open! And it really
-- is a one line change, unlike the "if/else" in Ruby!
container n = "bottles"

through :: Int -> Int -> [Int]
through n1 n2 =
  if n1 > n2 then [n1, n1-1..n2]
             else [n1..n2]

-- ugh reverse ranges so ugly -- I'll make a helper copying Ruby's `downto`.
-- but what happens if n1 isn't greater than n2?
-- this makes me think about edgecases and error conditions, which 99 bottles hasn't discussed yet.
-- tried fixing this with various type constraints -- VerseNumber and VerseRange and mkVerseBlah
-- but in the end, just made `through` work going up or down :)

verses :: Int -> Int -> Text
verses v1 v2 =
  T.intercalate "\n\n" $ map verse (v1 `through` v2)

song :: Text
song = verses 99 0
