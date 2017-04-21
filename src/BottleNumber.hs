{-# LANGUAGE OverloadedStrings #-}

module BottleNumber where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

tshow :: Show a => a -> Text
tshow = T.pack . show

data BottleNumber =
  BottleNumber { container' :: Text
                , quantity' :: Text
                , action'   :: Text
                , next'     :: BottleNumber }

instance Show BottleNumber where
  show bn = T.unpack $ quantity' bn <> " " <> container' bn

-- we have three cases in the flocked five -- 0, 1, and n
-- so make three cases
-- I probably should've made verse use BottleNumber sooner
-- so the tests were meaningful earlier.

-- Now I want to get rid of the "BottleNumber"s in mkBottleNumber,
-- but I can't because action is still using pronoun.
-- What if I inline the pronoun?

mkBottleNumber :: Int -> BottleNumber
mkBottleNumber n@0 =
  BottleNumber { container' = "bottles"
                , quantity' = "no more"
                , action' = "Go to the store and buy some more"
                , next' = mkBottleNumber 99 }
mkBottleNumber n@1 =
  BottleNumber { container' = "bottle"
                , quantity' = tshow n
                , action' = "Take it down and pass it around"
                , next' = mkBottleNumber (n - 1) }
mkBottleNumber n =
  BottleNumber { container' = "bottles"
                , quantity' = tshow n
                , action' = "Take one down and pass it around"
                , next' = mkBottleNumber (n - 1) }
