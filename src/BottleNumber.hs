{-# LANGUAGE OverloadedStrings #-}

module BottleNumber where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

tshow :: Show a => a -> Text
tshow = T.pack . show

newtype BottleNumber = BottleNumber { bNumber :: Int }

container :: BottleNumber -> Text
container (BottleNumber 1) = "bottle"
container n = "bottles"

pronoun :: Int -> Text
pronoun 1 = "it"
pronoun n = "one"

-- I have to worry about converting `n` to a string, but sandi doesn't
quantity :: BottleNumber -> Text
quantity (BottleNumber 0) = "no more"
quantity (BottleNumber n) = tshow n

action :: BottleNumber -> Text
action (BottleNumber 0) = "Go to the store and buy some more"
action (BottleNumber n) = "Take " <> pronoun n <> " down and pass it around"

next :: BottleNumber -> BottleNumber
next (BottleNumber 0) = BottleNumber 99
next (BottleNumber n) = BottleNumber (n - 1)
