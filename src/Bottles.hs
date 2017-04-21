{-# LANGUAGE OverloadedStrings #-}

module Bottles where

import           Data.Char    (toUpper)
import           Data.Monoid  ((<>))
import           Data.Text    (Text)
import qualified Data.Text    as T

import           BottleNumber

capitalize :: Text -> Text
capitalize "" = ""
capitalize words = toUpper (T.head words) `T.cons` T.tail words

verse :: Int -> Text
verse n =
  let bn = mkBottleNumber n in
    capitalize (tshow bn) <> " of beer on the wall, " <>
    tshow bn <> " of beer. \n" <>
    action bn <> ", " <>
    tshow (next bn) <> " of beer on the wall."

through :: Int -> Int -> [Int]
through n1 n2 =
  if n1 > n2 then [n1, n1-1..n2]
             else [n1..n2]

verses :: Int -> Int -> Text
verses v1 v2 =
  T.intercalate "\n\n" $ map verse (v1 `through` v2)

song :: Text
song = verses 99 0
