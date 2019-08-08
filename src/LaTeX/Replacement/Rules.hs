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

fractional3_3Rep :: ReplacementData
fractional3_3Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})[\.,](\d{1,3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:s4:s5:s6:_) -> s1 <> "\\," <> s2 <> "\\," <> s3 <> "{,}" <> s4 <> s5 <> s6)

fractional2_3Rep :: ReplacementData
fractional2_3Rep = Replacement
  [re|(\d{1,3})~*(\d{3})[\.,](\d{1,3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:s4:s5:_) -> s1 <> "\\," <> s2 <> "{,}" <> s3 <> s4 <> s5)

fractional1_3Rep :: ReplacementData
fractional1_3Rep = Replacement
  [re|(\d{1,3})[\.,](\d{1,3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:s4:_) -> s1 <> "{,}" <> s2 <> s3 <> s4)

fractional3_2Rep :: ReplacementData
fractional3_2Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})[\.,](\d{1,3})~*(\d{3})|]
  (\(s1:s2:s3:s4:s5:_) -> s1 <> "\\," <> s2 <> "\\," <> s3 <> "{,}" <> s4 <> s5)

fractional3_1Rep :: ReplacementData
fractional3_1Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})[\.,](\d{1,3})|]
  (\(s1:s2:s3:s4:_) -> s1 <> "\\," <> s2 <> "\\," <> s3 <> "{,}" <> s4)

fractional2_2Rep :: ReplacementData
fractional2_2Rep = Replacement
  [re|(\d{1,3})~*(\d{3})[\.,](\d{1,3})~*(\d{3})|]
  (\(s1:s2:s3:s4:_) -> s1 <> "\\," <> s2 <> "{,}" <> s3 <> s4)

fractional2_1Rep :: ReplacementData
fractional2_1Rep = Replacement
  [re|(\d{1,3})[\.,](\d{1,3})~*(\d{3})|]
  (\(s1:s2:s3:_) -> s1 <> "{,}" <> s2 <> s3)

fractional1_2Rep :: ReplacementData
fractional1_2Rep = Replacement
  [re|(\d{2,3})~*(\d{3})[\.,](\d{1,3})|]
  (\(s1:s2:s3:_) -> s1 <> "\\," <> s2 <> "{,}" <> s3)

fractional1_1Rep :: ReplacementData
fractional1_1Rep = Replacement
  [re|(\d{1,4})[\.,](\d{1,3})|]
  (\(s1:s2:_) -> s1 <> "{,}" <> s2)

timeMRep :: ReplacementData
timeMRep = Replacement
  [re|(\d{1,2}):(\d{1,2})|]
  (\(s1:s2:_) -> s1 <> ":" <> s2)

timeMsRep :: ReplacementData
timeMsRep = Replacement
  [re|(\d{1,2}):(\d{1,2}):(\d{1,2}):(\d{1,3})|]
  (\(s1:s2:s3:s4:_) -> s1 <> ":" <> s2 <> ":" <> s3 <> ":" <> s4)

timeSRep :: ReplacementData
timeSRep = Replacement
  [re|(\d{1,2}):(\d{1,2}):(\d{1,2})|]
  (\(s1:s2:s3:_) -> s1 <> ":" <> s2 <> ":" <> s3)

integer5Rep :: ReplacementData
integer5Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:s4:s5:_) -> s1 <> "\\," <> s2 <> "\\," <> s3 <> "\\," <> s4 <> "\\," <> s5)

integer4Rep :: ReplacementData
integer4Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:s4:_) -> s1 <> "\\," <> s2 <> "\\," <> s3 <> "\\," <> s4)

integer3Rep :: ReplacementData
integer3Rep = Replacement
  [re|(\d{1,3})~*(\d{3})~*(\d{3})|]
  (\(s1:s2:s3:_) -> s1 <> "\\," <> s2 <> "\\," <> s3)

integer2Rep :: ReplacementData
integer2Rep = Replacement
  [re|(\d{2,3})~*(\d{3})|]
  (\(s1:s2:_) -> s1 <> "\\," <> s2)

integer1Rep :: ReplacementData
integer1Rep = Replacement [re|(\d{1,4})|] head

mathBracketsRep :: ReplacementData
mathBracketsRep = Replacement [re|\\\((.*?)\\\)|] head

mathDollarsRep :: ReplacementData
mathDollarsRep = Replacement [re|\$(.*?)\$|] head
