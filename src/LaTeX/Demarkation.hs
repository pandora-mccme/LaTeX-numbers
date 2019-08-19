{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Demarkation where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

import LaTeX.Types
import LaTeX.Utils

-- $setup
-- >>> :set -XOverloadedStrings

-- Idea: odd indices are to be in dictionary. See doctest.

{- $
-- >>> splitByRegex defaultMathModeDictionary "text $3$ with some $dfrac{1}{2}$ formula"
-- ["text ","$3$"," with some ","$dfrac{1}{2}$"," formula"]
-- >>> splitByRegex defaultMathModeDictionary "$3$ with some $dfrac{1}{2}$"
-- ["","$3$"," with some ","$dfrac{1}{2}$",""]
-}
splitByRegex :: Dictionary -> Text -> [Text]
splitByRegex (Dictionary dict) txt = T.splitOn "REPLACE"
                                   $ replaceWithList addReplaces dict txt

tagAsNorm :: Mode -> [Text] -> [Tagged Text]
tagAsNorm _ [] = []
tagAsNorm NormalMode xs = map (\x -> (Tagged x NormalMode)) xs
tagAsNorm nextMode (x:xs) = (Tagged x NormalMode): tagAs nextMode xs

tagAs :: Mode -> [Text] -> [Tagged Text]
tagAs _ [] = []
tagAs mode (x:xs) = (Tagged x mode): tagAsNorm mode xs

-- Here is very strong assumption -- there can not be any `textbf`, `textit` and commands from dictionary inside math mode.
mark :: Mode -> Dictionary -> Tagged Text -> [Tagged Text]
mark wantedMode dict (Tagged a NormalMode) = tagAsNorm wantedMode (splitByRegex dict a)
mark _ _ (Tagged a mode) = [Tagged a mode]

markCommands :: Dictionary -> Tagged Text -> [Tagged Text]
markCommands dict = mark CMD (defaultCmdDictionary <> dict)

markMathModeExt :: Dictionary -> Tagged Text -> [Tagged Text]
markMathModeExt mathDict = mark MathMode (defaultMathModeDictionary <> mathDict)

markMathMode :: Tagged Text -> [Tagged Text]
markMathMode = mark MathMode defaultMathModeDictionary

markBold :: Tagged Text -> [Tagged Text]
markBold = mark Bold boldDictionary

markItalic :: Tagged Text -> [Tagged Text]
markItalic = mark Italic italicDictionary
