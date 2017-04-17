{-# LANGUAGE OverloadedStrings #-}

module Bottles where

import           Data.Monoid ((<>))
import           Data.Text   (Text)
import qualified Data.Text   as T


tshow :: Show a => a -> Text
tshow = T.pack . show

-- notes: I have to worry about converting `n` to a string, but sandi doesn't

verse :: Int -> Text
verse 0 =
  "No more bottles of beer on the wall, \
  \no more bottles of beer. \n\
  \Go to the store and buy some more, \
  \99 bottles of beer on the wall."
verse 1 =
  "1 bottle of beer on the wall, \
  \1 bottle of beer. \n\
  \Take it down and pass it around, \
  \No more bottles of beer on the wall."
verse 2 =
  "2 bottles of beer on the wall, \
  \2 bottles of beer. \n\
  \Take one down and pass it around, \
  \1 bottle of beer on the wall."
verse n =
  tshow n <> " bottles of beer on the wall, " <>
  tshow n <> " bottles of beer. \n" <>
  "Take one down and pass it around, " <>
  tshow (n-1) <> " bottles of beer on the wall."
