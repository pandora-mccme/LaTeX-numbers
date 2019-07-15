module LaTeX.Demarkation where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Data.Text.ICU (Regex, regex)
import Data.Text.ICU.Replace (replaceAll)

import Data.List.Split (splitWhen)

import LaTeX.Types

trimEnds :: [Text] -> Maybe Trimmed
trimEnds content = case a of
    [h,b,t] -> Just $ Trimmed h b t
    _ -> Nothing
  where
    a = map (T.intercalate "\n") $ splitWhen isBeginEnd content

isBeginEnd :: Text -> Bool
isBeginEnd a =
     "\\begin{document}" `T.isInfixOf` a
  || "\\end{document}" `T.isInfixOf` a

readRegex :: Text -> Regex
readRegex = regex []
          -- Maybe I don't need it. I am already obliged to use escaping in dictionary.
          . T.replace "}" "\\}"
          . T.replace "{" "\\{"
          . T.replace "[" "\\["
          . T.replace "]" "\\]"
          . (\t -> "(" <> t <> ")")

splitByRegex :: Dictionary -> Text -> [Text]
splitByRegex (Dictionary dict) txt =
  T.splitOn "%%%%%" $ foldl (.) id (map (\pattern -> replaceAll pattern "%%%%%$1%%%%%") dict) txt

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
