{-# LANGUAGE OverloadedStrings #-}

module BottleNumber where

import           Data.Monoid
import           Data.Text   (Text)
import qualified Data.Text   as T

tshow :: Show a => a -> Text
tshow = T.pack . show

data BottleNumber =
  BottleNumber { container :: Text
               , quantity  :: Text
               , action    :: Text
               , next      :: BottleNumber }

instance Show BottleNumber where
  show bn = T.unpack $ quantity bn <> " " <> container bn

mkBottleNumber :: Int -> BottleNumber
mkBottleNumber 0 =
  BottleNumber { container = "bottles"
               , quantity = "no more"
               , action = "Go to the store and buy some more"
               , next = mkBottleNumber 99 }
mkBottleNumber 1 =
  BottleNumber { container = "bottle"
               , quantity = "1"
               , action = "Take it down and pass it around"
               , next = mkBottleNumber 0 }
mkBottleNumber n =
  BottleNumber { container = "bottles"
               , quantity = tshow n
               , action = "Take one down and pass it around"
               , next = mkBottleNumber (n - 1) }
