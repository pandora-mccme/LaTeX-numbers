{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Demarkation where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split (splitWhen)
import Data.String.Conversions (cs)

import Text.Regex.PCRE.Heavy

import LaTeX.Types
import LaTeX.Replacement.Procedures

-- $setup
-- >>> :set -XOverloadedStrings

trimEmptyEnds :: Text -> Text
trimEmptyEnds = gsub [re|\s+$|] (""::Text)

trimEnds :: [Text] -> Maybe Trimmed
trimEnds content = case a of
    [h,b,t] -> Just $ Trimmed h b t
    _ -> Nothing
  where
    a = map (T.intercalate "\n") $ splitWhen isBeginEnd $ map trimEmptyEnds content

isBeginEnd :: Text -> Bool
isBeginEnd a =
     "\\begin{document}" `T.isInfixOf` a
  || "\\end{document}" `T.isInfixOf` a

-- $
-- >>> makeReplacer "s" "tests"
-- "te\\st\\s"
makeReplacer :: Text -> Text -> Text
makeReplacer symbol = T.replace symbol ("\\" <> symbol)

-- $
-- >>> replaceSpecials "\\problem{S_1_1_1*}"
-- "\\\\problem\\{S_1_1_1\\*\\}"
replaceSpecials :: Text -> Text
replaceSpecials = (foldl1 (.) spec) . T.replace "\\" "\\\\"
-- Full list of regex special characters
  where spec = map makeReplacer ["*",".","?","{","}","[","]","(",")","$","^","+","|"]

{- $
-- >>> readRegex False "\\raisebox{##}[##][##]"
-- Just (Regex "(\\\\raisebox\\{(?s).*?\\}\\[(?s).*?\\]\\[(?s).*?\\])")
-- >>> readRegex False "\\begin{align*}##\\end{align*}"
-- Just (Regex "(\\\\begin\\{align\\*\\}(?s).*?\\\\end\\{align\\*\\})")
-- >>> readRegex True "\\\\begin\\{align\\**\\}(?s).*?\\\\end\\{align\\**\\}"
-- Just (Regex "(\\\\begin\\{align\\**\\}(?s).*?\\\\end\\{align\\**\\})")
-}
readRegex :: Bool -> Text -> Either String Regex
readRegex False = flip compileM [] . cs . T.replace "#$" ".*?" . T.replace "##" "(?s).*?" . (\t -> "(" <> t <> ")") . replaceSpecials
readRegex True = flip compileM [] . cs . (\t -> "(" <> t <> ")")

-- Idea: odd indices are to be in dictionary. See doctest.

{- $
-- >>> splitByRegex defaultMathModeDictionary "text $3$ with some $dfrac{1}{2}$ formula"
-- ["text ","$3$"," with some ","$dfrac{1}{2}$"," formula"]
-- >>> splitByRegex defaultMathModeDictionary "$3$ with some $dfrac{1}{2}$"
-- ["","$3$"," with some ","$dfrac{1}{2}$",""]
-}
splitByRegex :: Dictionary -> Text -> [Text]
splitByRegex (Dictionary dict) txt =
  T.splitOn "REPLACE" $ foldl (.) id (map (\pattern -> replaceAll (Replacement pattern (\(s:_) -> "REPLACE" <> s <> "REPLACE"))) dict) txt

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
