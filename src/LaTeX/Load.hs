{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Load where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split (splitWhen)
import Data.String.Conversions (cs)

import Text.Regex.PCRE.Heavy

import LaTeX.Types

-- $setup
-- >>> :set -XOverloadedStrings

trimEmptyEnds :: Text -> Text
trimEmptyEnds = gsub [re|\s+$|] (""::Text)

removeEmptyLines :: Text -> Text
removeEmptyLines = gsub [re|(\n\n\n+|\n+$)|] ("\n"::Text)

trimEnds :: [Text] -> Maybe Trimmed
trimEnds content = case a of
    [h,b,t] -> Just $ Trimmed h b t
    _ -> Nothing
  where
    a = map (removeEmptyLines . T.intercalate "\n")
      $ splitWhen isBeginEnd
      $ map trimEmptyEnds content

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
readRegex False = flip compileM [] . cs
                . T.replace "#$" ".*?"
                . T.replace "##" "(?s).*?"
                . (\t -> "(" <> t <> ")")
                . replaceSpecials
readRegex True = flip compileM [] . cs
               . (\t -> "(" <> t <> ")")
