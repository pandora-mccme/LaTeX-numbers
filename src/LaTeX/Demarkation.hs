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
          . T.replace "}" "\\}"
          . T.replace "{" "\\{"
          . T.replace "[" "\\["
          . T.replace "]" "\\]"
          . (\t -> "(\\" <> t <> ")")

splitByRegex :: Dictionary -> Text -> [Text]
splitByRegex (Dictionary dict) txt =
  T.splitOn "#^#^#^#^#" $ foldl (.) id (map (\pattern -> replaceAll pattern "#^#^#^#^#$1#^#^#^#^#") dict) txt

-- tagAsNorm (splitByRegex pat bs)
tagAsNorm :: Mode -> [Text] -> [Tagged Text]
tagAsNorm _ [] = []
tagAsNorm NormalMode txt = map (\a -> Tagged a NormalMode) txt
tagAsNorm CMD (x:xs) = (Tagged x NormalMode): tagAsCmd xs
tagAsNorm MathMode (x:xs) = (Tagged x NormalMode): tagAsCmd xs

tagAsCmd :: [Text] -> [Tagged Text]
tagAsCmd [] = []
tagAsCmd (x:xs) = (Tagged x CMD): tagAsNorm CMD xs

tagAsMath :: [Text] -> [Tagged Text]
tagAsMath [] = []
tagAsMath (x:xs) = (Tagged x MathMode): tagAsNorm MathMode xs

markCommands :: Dictionary -> Tagged Text -> [Tagged Text]
markCommands _dict (Tagged a CMD) = [Tagged a CMD]
markCommands _dict (Tagged a MathMode) = [Tagged a MathMode]
markCommands dict (Tagged a NormalMode) = tagAsNorm CMD (splitByRegex dict a)

markMathMode :: Dictionary -> Tagged Text -> [Tagged Text]
markMathMode _dict (Tagged a CMD) = [Tagged a CMD]
markMathMode _dict (Tagged a MathMode) = [Tagged a MathMode]
markMathMode dict (Tagged a NormalMode) = tagAsNorm MathMode (splitByRegex dict a)
