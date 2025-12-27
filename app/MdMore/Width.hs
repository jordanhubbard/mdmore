module MdMore.Width
  ( displayWidth
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char.WCWidth (wcwidth)

displayWidth :: Text -> Int
displayWidth = sum . map charWidth . T.unpack
  where
    charWidth c =
      let w = wcwidth c
       in if w < 0 then 1 else w
