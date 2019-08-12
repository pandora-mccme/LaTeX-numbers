{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Replacement.Rules where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

import LaTeX.Types

commaRep :: ReplacementData
commaRep = Replacement
  [re|(\d)\{,\}(\d)|]
  (\(s1:s2:_) -> s1 <> "," <> s2)

spaceRep :: ReplacementData
spaceRep = Replacement
  [re|(\d)\\,(\d)|]
  (\(s1:s2:_) -> s1 <> s2)

listRep :: ReplacementData
listRep = Replacement
  [re|(\d[,;]\d(?:,\d)+)|]
  (\[d] -> T.replace "," ", " d)

addSpaces :: Text -> Text
addSpaces txt = if T.length txt == 4
  then noTildes txt
  else spaces . noTildes $ txt
  where
    spaces = T.intercalate "\\," . map T.reverse . reverse . T.chunksOf 3 . T.reverse
    noTildes = T.replace "~" ""

fractionalRep :: ReplacementData
fractionalRep = Replacement
  [re|(\d[\d~]*)[\.,]([\d~]*\d)|]
  (\(s1:s2:_) -> addSpaces s1 <> "{,}" <> s2)

integerRep :: ReplacementData
integerRep = Replacement
  [re|(\d[\d~]*\d|\d)|]
  (\(s1:_) -> addSpaces s1)

timeRep :: ReplacementData
timeRep = Replacement
  [re|(\d{1,2}:\d{2}:\d{2}:\d{3}|\d{1,2}:\d{2}:\d{2}|\d{1,2}:\d{2})|] head

mathBracketsRep :: ReplacementData
mathBracketsRep = Replacement [re|\\\((.*?)\\\)|] head

mathDollarsRep :: ReplacementData
mathDollarsRep = Replacement [re|\$(.*?)\$|] head
