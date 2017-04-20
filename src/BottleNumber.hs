{-# LANGUAGE OverloadedStrings #-}

module BottleNumber where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

tshow :: Show a => a -> Text
tshow = T.pack . show

-- can't take temporarily optional parameter
container :: Int -> Text
container 1 = "bottle"
-- it's nice that this is VERY Open! And it really
-- is a one line change, unlike the "if/else" in Ruby!
container n = "bottles"

pronoun :: Int -> Text
pronoun 1 = "it"
pronoun n = "one"

-- I have to worry about converting `n` to a string, but sandi doesn't
quantity :: Int -> Text
quantity 0 = "no more"
quantity n = tshow n

action :: Int -> Text
action 0 = "Go to the store and buy some more"
action n = "Take " <> pronoun n <> " down and pass it around"

next :: Int -> Int
next 0 = 99
next n = n - 1
