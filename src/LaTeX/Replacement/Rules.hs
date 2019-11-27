{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Replacement.Rules where

import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

import LaTeX.Types
import LaTeX.Utils

commaRep :: ReplacementData
commaRep = Replacement
  [re|(\d)\{,\}(\d)|]
  (\(s1:s2:_) -> s1 <> "," <> s2)

colonRep :: ReplacementData
colonRep = Replacement
  [re|(\d)\{:\}(\d)|]
  (\(s1:s2:_) -> s1 <> ":" <> s2)

spaceRep :: ReplacementData
spaceRep = Replacement
  [re|(\d)\\,(\d)|]
  (\(s1:s2:_) -> s1 <> s2)

listRep :: ReplacementData
listRep = Replacement
  [re|(\d[,;]\d(?:,\d)+)|]
  (addListSpaces . head)

fractionalRep :: ReplacementData
fractionalRep = Replacement
  [re|(\d[\d~]*)[\.,]([\d~]*\d)|]
  (\(s1:s2:_) -> addNumericSpaces s1 <> "{,}" <> s2)

integerRep :: ReplacementData
integerRep = Replacement
  [re|(\d[\d~]*\d(\\%)?|\d(\\%)?)|]
  (\(s1:_) -> addNumericSpaces s1)

timeRep :: ReplacementData
timeRep = Replacement
  [re|(\d{1,2}:\d{2}:\d{2}:\d{3}|\d{1,2}:\d{2}:\d{2}|\d{1,2}:\d{2})|]
  (T.replace ":" "{:}" . head)

mathRep :: ReplacementData
mathRep = Replacement
  [re|(?:\\\(|\$)(.*?)(?:\\\)|\$)|]
  head
