{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Replacement.Procedures where

import Data.Monoid ((<>))

import qualified Data.Text as T
import Data.Text (Text)

import Text.Regex.PCRE.Heavy

import LaTeX.Replacement.Rules
import LaTeX.Types
import LaTeX.Utils

-- $setup
-- >>> :set -XOverloadedStrings

toMathMode :: ReplacementData -> ReplacementData
toMathMode Replacement{..} = Replacement replacementPattern ((\s -> "$" <> s <> "$") . replacementResult)

toItalic :: ReplacementData -> ReplacementData
toItalic Replacement{..} = Replacement replacementPattern ((\s -> "$ \\mathit{" <> s <> "} $") . replacementResult)

toBold :: ReplacementData -> ReplacementData
toBold Replacement{..} = Replacement replacementPattern ((\s -> "$ \\mathbf{" <> s <> "} $") . replacementResult)

replaceAll :: ReplacementData -> Text -> Text
replaceAll Replacement{..} = gsub replacementPattern replacementResult

modifier :: Mode -> ReplacementData -> ReplacementData
modifier MathMode = id
modifier NormalMode = toMathMode
modifier CMD = id
modifier Italic = toItalic
modifier Bold = toBold

-- |
-- >>> commonReplacement MathMode integerRep "1 22 334 4444 55555 666666 7777777 32,34"
-- "1 22 334 4444 55\\,555 666\\,666 7\\,777\\,777 32,34"
-- >>> commonReplacement NormalMode integerRep "1 22 334 4444 55555 666666 7777777 32\\%,34"
-- "$1$ $22$ $334$ $4444$ $55\\,555$ $666\\,666$ $7\\,777\\,777$ $32\\%$,$34$"
-- >>> commonReplacement NormalMode timeRep "11:21 11:21:22 11:21:22:111"
-- "$11{:}21$ $11{:}21{:}22$ $11{:}21{:}22{:}111$"
-- >>> commonReplacement MathMode fractionalRep "1,23 1.23 1{,}23 1.34555 1111.23 111111.3 1111111{,}3 1111111,3 d,d 4444,32"
-- "1{,}23 1{,}23 1{,}23 1{,}34555 1111{,}23 111\\,111{,}3 1111111{,}3 1\\,111\\,111{,}3 d,d 4444{,}32"
-- >>> commonReplacement NormalMode fractionalRep "1,23 1.23 1{,}23 1.34555 1111.23 111111.3 1111111{,}3 1111111,3 d,d"
-- "$1{,}23$ $1{,}23$ 1{,}23 $1{,}34555$ $1111{,}23$ $111\\,111{,}3$ 1111111{,}3 $1\\,111\\,111{,}3$ d,d"
-- >>> commonReplacement NormalMode mathRep "\\(33,22\\) $33,22$"
-- "$33,22$ $33,22$"
commonReplacement :: Mode -> ReplacementData -> Text -> Text
commonReplacement mode rep = replaceAll (modifier mode rep)

-- |
-- >>> clearFormattingReplacement "11{,}21 11{,}2 22{,}2 2{,} 2,2 d{,}d 2\\,2 a\\,2 \\, {,} , 2\\, \\,2 1,2 1,2,3,4"
-- "11,21 11,2 22,2 2{,} 2,2 d{,}d 22 a\\,2 \\, {,} , 2\\, \\,2 1,2 1, 2, 3, 4"
clearFormattingReplacement :: Text -> Text
clearFormattingReplacement = replaceAll spaceRep
                           . replaceAll commaRep
                           . replaceAll colonRep
                           . replaceAll listRep

clearCyrillicReplacement :: Text -> Text
clearCyrillicReplacement = foldl (flip (.)) id $
  [ T.replace "с" "c"
  , T.replace "С" "C"
  , T.replace "В" "B"
  , T.replace "а" "a"
  , T.replace "А" "A"
  , T.replace "Н" "H"
  , T.replace "о" "o"
  , T.replace "О" "O"
  , T.replace "р" "p"
  , T.replace "Р" "P"
  , T.replace "е" "e"
  , T.replace "Е" "E"
  , T.replace "Т" "T"
  ]

tildeReplacement :: Dictionary -> Text -> Text
tildeReplacement (Dictionary tildes) txt = replaceWithList addTilde tildes txt

-- First arg -- mode to operate in.
fractionalUpdateInner :: Mode -> Tagged Text -> Tagged Text
fractionalUpdateInner NormalMode (Tagged content MathMode) = (Tagged content MathMode)
fractionalUpdateInner MathMode (Tagged content NormalMode) = (Tagged content NormalMode)
fractionalUpdateInner NormalMode (Tagged content NormalMode) = (Tagged (commonReplacement NormalMode fractionalRep content) NormalMode)
fractionalUpdateInner MathMode (Tagged content MathMode) = (Tagged (commonReplacement MathMode fractionalRep content) MathMode)
fractionalUpdateInner _ a = a

fractionalNormalUpdate :: Tagged Text -> Tagged Text
fractionalNormalUpdate = fractionalUpdateInner NormalMode

fractionalMathUpdate :: Tagged Text -> Tagged Text
fractionalMathUpdate = fractionalUpdateInner MathMode

mathSpecialUpdate :: Mode -> Tagged Text -> Tagged Text
mathSpecialUpdate Italic (Tagged content Italic) = (Tagged (commonReplacement Italic mathRep content) Italic)
mathSpecialUpdate Italic (Tagged content a) = (Tagged content a)
mathSpecialUpdate Bold (Tagged content Bold) = (Tagged (commonReplacement Bold mathRep content) Bold)
mathSpecialUpdate Bold (Tagged content a) = (Tagged content a)
mathSpecialUpdate _ (Tagged content a) = (Tagged content a)

integerUpdateInner :: Mode -> ReplacementData -> Tagged Text -> Tagged Text
integerUpdateInner MathMode _rep (Tagged content NormalMode) = (Tagged content NormalMode)
integerUpdateInner MathMode rep (Tagged content MathMode) = (Tagged (commonReplacement MathMode rep content) MathMode)
integerUpdateInner NormalMode rep (Tagged content NormalMode) = (Tagged (commonReplacement NormalMode rep content) NormalMode)
integerUpdateInner NormalMode _rep (Tagged content MathMode) = (Tagged content MathMode)
integerUpdateInner _ _rep a = a

integerNormalUpdate :: Tagged Text -> Tagged Text
integerNormalUpdate = integerUpdateInner NormalMode integerRep

integerMathUpdate :: Tagged Text -> Tagged Text
integerMathUpdate = integerUpdateInner MathMode integerRep

clearCyrillic :: Tagged Text -> Tagged Text
clearCyrillic (Tagged txt MathMode) = Tagged (clearCyrillicReplacement txt) MathMode
clearCyrillic a = a

clearFormatting :: Tagged Text -> Tagged Text
clearFormatting (Tagged txt NormalMode) = Tagged (clearFormattingReplacement txt) NormalMode
clearFormatting (Tagged txt MathMode) = Tagged (clearFormattingReplacement txt) MathMode
clearFormatting a = a

tildeUpdate :: Dictionary -> Tagged Text -> Tagged Text
tildeUpdate tildes (Tagged txt a) = Tagged (tildeReplacement tildes txt) a

timeUpdate :: Tagged Text -> Tagged Text
timeUpdate (Tagged txt NormalMode) = Tagged (commonReplacement NormalMode timeRep txt) NormalMode
timeUpdate a = a

-- Assuming there is practically never $\textit{}$.
-- replace by @mathModeDictionary regexps@. All text in math mode inside.
mathItalicUpdate :: Tagged Text -> Tagged Text
mathItalicUpdate = mathSpecialUpdate Italic

mathBoldUpdate :: Tagged Text -> Tagged Text
mathBoldUpdate = mathSpecialUpdate Bold
