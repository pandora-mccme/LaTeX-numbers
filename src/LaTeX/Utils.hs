{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Utils where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

addNumericSpaces :: Text -> Text
addNumericSpaces txt = if T.length txt == 4
  then noTildes txt
  else spaces . noTildes $ txt
  where
    spaces = T.intercalate "\\," . map T.reverse . reverse . T.chunksOf 3 . T.reverse
    noTildes = T.replace "~" ""

addListSpaces :: Text -> Text
addListSpaces = T.replace "," ", "

isPattern :: Text -> Bool
isPattern l = (not $ T.isPrefixOf "-- " l)
           && (l /= "")
           && (not $ T.all isSpace l)
