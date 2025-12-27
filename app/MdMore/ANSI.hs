{-# LANGUAGE OverloadedStrings #-}

module MdMore.ANSI
  ( reset
  , bold
  , dim
  , italic
  , underline
  , reverseVideo
  , fg256
  , bg256
  )
where

import Data.Text (Text)
import qualified Data.Text as T

reset :: Text
reset = "\ESC[0m"

bold :: Text
bold = "\ESC[1m"

dim :: Text
dim = "\ESC[2m"

italic :: Text
italic = "\ESC[3m"

underline :: Text
underline = "\ESC[4m"

reverseVideo :: Text
reverseVideo = "\ESC[7m"

fg256 :: Int -> Text
fg256 n = "\ESC[38;5;" <> T.pack (show n) <> "m"

bg256 :: Int -> Text
bg256 n = "\ESC[48;5;" <> T.pack (show n) <> "m"
