{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Utils where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

outputExtensionDebug :: Text
outputExtensionDebug = "test"

outputExtension :: Text
outputExtension = "tex"

chooseExtension :: Bool -> Text
chooseExtension True = outputExtensionDebug
chooseExtension False = outputExtension

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

replaceWithList :: (Text -> Text) -> [Regex] -> Text -> Text
replaceWithList f dict = foldl (.) id (map (makeRep f) dict)

makeRep :: (Text -> Text) -> Regex -> Text -> Text
makeRep f pattern = gsub pattern f

addReplaces :: Text -> Text
addReplaces s = "REPLACE" <> s <> "REPLACE"

makeTildeLeftPattern :: Text -> Text
makeTildeLeftPattern t = "( " <> t <> ")(?![а-яА-Я])"

makeTildeRightPattern :: Text -> Text
makeTildeRightPattern t = "(?<![а-яА-Я])(" <> t <> " )"

addTilde :: Text -> Text
addTilde = T.replace " " "~"
