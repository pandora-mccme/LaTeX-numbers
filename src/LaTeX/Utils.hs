{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Utils where

import Data.Char
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

-- $setup
-- >>> :set -XOverloadedStrings

multilines :: Text
multilines = "MULTILINES"

outputExtensionDebug :: Text
outputExtensionDebug = "test"

outputExtension :: Text
outputExtension = "tex"

chooseExtension :: Bool -> Text
chooseExtension True = outputExtensionDebug
chooseExtension False = outputExtension

-- |
-- >>> addNumericSpaces "3333"
-- "3333"
-- >>> addNumericSpaces "3333333"
-- "3\\,333\\,333"
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
replaceWithList f dict = foldl (flip (.)) id (map (makeRep f) dict)

makeRep :: (Text -> Text) -> Regex -> Text -> Text
makeRep f pattern = gsub pattern f

addReplaces :: Text -> Text
addReplaces s = "REPLACE" <> (T.replace "REPLACE" "" s) <> "REPLACE"

makeTildeLeftPattern :: Text -> Text
-- FIXME: а-яА-Я does not match all russian letters.
makeTildeLeftPattern t = "( " <> t <> ")(?![абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ])"

makeTildeRightPattern :: Text -> Text
makeTildeRightPattern t = "(?<![абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ])(" <> t <> " )"

makeTildeMidPattern :: Text -> Text
makeTildeMidPattern t = "(?<![абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ])("
                     <> t <> ")(?![абвгдеёжзийклмнопрстуфхцчшщъыьэюяАБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ])"

addTilde :: Text -> Text
addTilde = T.replace " " "~"

-- |
-- >>> replaceProblemTags 1 "\\problem{s}    \\problem{d}" "pure.tex"
-- "\\problem{pure_1}    \\problem{pure_2}"
replaceProblemTags :: Int -> Text -> String -> Text
replaceProblemTags num txt filename = if "\\problem{" `T.isInfixOf` txt
  then start <> "\n\\problem{" <> problemName <> "_" <> T.pack (show num) <> replaceProblemTags (num+1) next filename
  else txt
  where
    (start, prNext) = T.breakOn "\\problem{" txt
    (_pr,next) = T.breakOn "}" prNext
    problemName = (flip (!!) 0) . T.splitOn "(" . fst . T.breakOn "." . T.pack $ filename
