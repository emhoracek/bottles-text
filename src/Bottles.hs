{-# LANGUAGE OverloadedStrings #-}

module Bottles where

import           Data.Char    (toUpper)
import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as T

import qualified BottleNumber as B

tshow :: Show a => a -> Text
tshow = T.pack . show

capitalize :: Text -> Text
capitalize "" = ""
capitalize words = toUpper (T.head words) `T.cons` T.tail words

-- Path is more like:
-- 1) doesn't compile ("Not in scope")
-- 2) Insert blah = undefined
-- 3) uncaught exception: ErrorCall (Prelude.undefined)
-- 4) add function signature and garbage result
-- 5) fix it


verse :: Int -> Text
-- changing the cases to make them the same one little bit at a time is the hardest part.
-- if I keep patternmatching, then get overlapping patterns
-- so first I'm changing this part to be a case statement
-- haskell getting cutesy again. I just want to capitalize the first letter, not title case,
-- so toTitle doesn't work! helper function time.
-- In Ruby, authors ran into a type error here!! So that is funny.. :)
-- Not sure how Liskov Substitution works here-- something to think about
verse n =
  let bn = B.BottleNumber n in
    capitalize (quantity n) <> " " <> container n <> " of beer on the wall, " <>
    quantity n <> " " <> container n <> " of beer. \n" <>
    action n <> ", " <>
    quantity (next n) <> " " <> container (next n) <> " of beer on the wall."

container :: Int -> Text
container n = B.container (B.BottleNumber n)

quantity :: Int -> Text
quantity n = B.quantity (B.BottleNumber n)

action :: Int -> Text
action n = B.action (B.BottleNumber n)

next :: Int -> Int
next n = B.bNumber (B.next (B.BottleNumber n))

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
