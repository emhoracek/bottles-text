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
                , next'      :: BottleNumber' }

instance Show BottleNumber' where
  show bn = T.unpack $ quantity' bn <> " " <> container' bn

toBottleNumber' :: BottleNumber -> BottleNumber'
toBottleNumber' (BottleNumber n) = mkBottleNumber n

-- we have three cases in the flocked five -- 0, 1, and n
-- so make three cases
-- I probably should've made verse use BottleNumber' sooner
-- so the tests were meaningful earlier.

mkBottleNumber :: Int -> BottleNumber'
mkBottleNumber n@0 =
  let bn = BottleNumber n in
      BottleNumber' { container' = "bottles"
                    , quantity' = "no more"
                    , action' = "Go to the store and buy some more"
                    , pronoun' = "one"
                    , next' = toBottleNumber' (BottleNumber 99) }
mkBottleNumber n@1 =
  let bn = BottleNumber n in
      BottleNumber' { container' = "bottle"
                    , quantity' = tshow n
                    , action' = "Take " <> pronoun bn <> " down and pass it around"
                    , pronoun' = "it"
                    , next' = toBottleNumber' (BottleNumber (n - 1)) }
mkBottleNumber n =
  let bn = BottleNumber n in
      BottleNumber' { container' = "bottles"
                    , quantity' = tshow n
                    , action' = "Take " <> pronoun bn <> " down and pass it around"
                    , pronoun' = "one"
                    , next' = toBottleNumber' (BottleNumber (n - 1)) }

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
