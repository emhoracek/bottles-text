{-# LANGUAGE OverloadedStrings #-}

module BottleNumber where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

tshow :: Show a => a -> Text
tshow = T.pack . show

newtype BottleNumber = BottleNumber { bNumber :: Int }

instance Show BottleNumber where
  show bn = T.unpack $ quantity bn <> " " <> container bn

data BottleNumber' =
  BottleNumber' { container' :: Text
                , quantity'  :: Text
                , action'    :: Text
                , pronoun'   :: Text
                , next'      :: BottleNumber }

instance Show BottleNumber' where
  show bn = T.unpack $ quantity' bn <> " " <> container' bn

mkBottleNumber :: Int -> BottleNumber'
mkBottleNumber n =
  let bn = BottleNumber n in
      BottleNumber' { container' = container bn
                    , quantity' = quantity bn
                    , action' = action bn
                    , pronoun' = pronoun bn
                    , next' = next bn }

container :: BottleNumber -> Text
container (BottleNumber 1) = "bottle"
container n = "bottles"

quantity :: BottleNumber -> Text
quantity (BottleNumber 0) = "no more"
quantity (BottleNumber n) = tshow n

action :: BottleNumber -> Text
action (BottleNumber 0) = "Go to the store and buy some more"
action bn = "Take " <> pronoun bn <> " down and pass it around"

pronoun :: BottleNumber -> Text
pronoun (BottleNumber 1) = "it"
pronoun n = "one"

next :: BottleNumber -> BottleNumber
next (BottleNumber 0) = BottleNumber 99
next (BottleNumber n) = BottleNumber (n - 1)
