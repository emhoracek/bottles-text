{-# LANGUAGE OverloadedStrings #-}

module Bottles where

import           Data.Char   (toUpper)
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
-- changing the cases to make them the same one little bit at a time is the hardest part.
-- if I keep patternmatching, then get overlapping patterns
-- so first I'm changing this part to be a case statement
-- haskell getting cutesy again. I just want to capitalize the first letter, not title case,
-- so toTitle doesn't work! helper function time.
-- In Ruby, authors ran into a type error here!! So that is funny.. :)
-- Not sure how Liskov Substitution works here-- something to think about

verse n =
  case n of
    0 -> capitalize (quantity n) <> " " <> container n <> " of beer on the wall, " <>
         quantity n  <> " " <> container n <> " of beer. \n" <>
         action n <> ", " <>
         quantity (next n) <> " bottles of beer on the wall."
    _ -> quantity n <> " " <> container n <> " of beer on the wall, " <>
         quantity n <> " " <> container n <> " of beer. \n" <>
         action n <> ", " <>
         quantity (next n) <> " " <> container (next n) <> " of beer on the wall."

capitalize :: Text -> Text
capitalize "" = ""
capitalize words = toUpper (T.head words) `T.cons` T.tail words

next :: Int -> Int
next 0 = 99
next n = n - 1

-- can't take temporarily optional parameter
container :: Int -> Text
container 1 = "bottle"
-- it's nice that this is VERY Open! And it really
-- is a one line change, unlike the "if/else" in Ruby!
container n = "bottles"

pronoun :: Int -> Text
pronoun 1 = "it"
pronoun n = "one"

quantity :: Int -> Text
quantity 0 = "no more"
quantity n = tshow n

action :: Int -> Text
action 0 = "Go to the store and buy some more"
action n = "Take " <> pronoun n <> " down and pass it around"

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
