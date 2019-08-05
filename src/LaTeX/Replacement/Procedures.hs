{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Replacement.Procedures where

import Data.Monoid ((<>))

import Data.Text (Text)

import Text.Regex.PCRE.Heavy

import LaTeX.Replacement.Rules
import LaTeX.Types

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

-- FIXME: @Replacement@ functions must not permit inapplicable @Mode@ arguments.

{- $
-- >>> mathSpecialReplacement NormalMode "\\(33,22\\)"
-- "$33,22$"
-- >>> mathSpecialReplacement NormalMode "$33,22$"
-- "$33,22$"
-}
mathSpecialReplacement :: Mode -> Text -> Text
mathSpecialReplacement mode =
    replaceAll (modifier mode mathBracketsRep)
  . replaceAll (modifier mode mathDollarsRep)

{- $
-- >>> commonReplacement MathMode integer2Rep "1 22 334 4444 55555 666666 7777777 32,34"
-- "1 22 334 4444 55\\,555 666\\,666 7777777 32,34"
-- >>> commonReplacement MathMode integer3Rep "1 22 334 4444 55555 666666 7777777 32,34"
-- "1 22 334 4444 55555 666666 7\\,777\\,777 32,34"
-- >>> commonReplacement NormalMode integer1Rep "1 22 334 4444 55555 666666 7777777 32,34"
-- "$1$ $22$ $334$ $4444$ 55555 666666 7777777 $32$,$34$"
-- >>> commonReplacement NormalMode timeShortRep "11:21"
-- "$11:21$"
-- >>> commonReplacement NormalMode timeLongRep "11:21:22"
-- "$11:21:22$"
-}
commonReplacement :: Mode -> ReplacementData -> Text -> Text
commonReplacement mode rep = replaceAll (modifier mode rep)

{- $
-
-- Doc: is always executed after @clearFormattingInner@
-- >>> fractionalReplacement MathMode "1,23 1.23 1{,}23 1.34555 1111.23 111111.3 1111111{,}3 1111111,3 d,d"
-- "1{,}23 1{,}23 1{,}23 1{,}34555 1111{,}23 111\\,111{,}3 1111111{,}3 1\\,111\\,111{,}3 d,d"
-- >>> fractionalReplacement NormalMode "1,23 1.23 1{,}23 1.34555 1111.23 111111.3 1111111{,}3 1111111,3 d,d"
-- "$1{,}23$ $1{,}23$ 1{,}23 $1{,}34555$ $1111{,}23$ $111\\,111{,}3$ 1111111{,}3 $1\\,111\\,111{,}3$ d,d"
-}
fractionalReplacement :: Mode -> Text -> Text
fractionalReplacement mode = foldr1 (.) $ map (\rep -> replaceAll (modifier mode rep)) reps
  where
    reps = [ fractional1_1Rep, fractional2_1Rep, fractional1_2Rep
           , fractional1_3Rep, fractional3_1Rep, fractional2_2Rep
           , fractional3_2Rep, fractional2_3Rep, fractional3_3Rep
           ]

-- First arg -- mode to operate in.
fractionalUpdateInner :: Mode -> Tagged Text -> Tagged Text
fractionalUpdateInner NormalMode (Tagged content MathMode) = (Tagged content MathMode)
fractionalUpdateInner MathMode (Tagged content NormalMode) = (Tagged content NormalMode)
fractionalUpdateInner NormalMode (Tagged content NormalMode) = (Tagged (fractionalReplacement NormalMode content) NormalMode)
fractionalUpdateInner MathMode (Tagged content MathMode) = (Tagged (fractionalReplacement MathMode content) MathMode)
fractionalUpdateInner _ a = a

fractionalNormalUpdate :: Tagged Text -> Tagged Text
fractionalNormalUpdate = fractionalUpdateInner NormalMode

fractionalMathUpdate :: Tagged Text -> Tagged Text
fractionalMathUpdate = fractionalUpdateInner MathMode

mathSpecialUpdate :: Mode -> Tagged Text -> Tagged Text
mathSpecialUpdate Italic (Tagged content Italic) = (Tagged (mathSpecialReplacement Italic content) Italic)
mathSpecialUpdate Italic (Tagged content a) = (Tagged content a)
mathSpecialUpdate Bold (Tagged content Bold) = (Tagged (mathSpecialReplacement Bold content) Bold)
mathSpecialUpdate Bold (Tagged content a) = (Tagged content a)
mathSpecialUpdate _ (Tagged content a) = (Tagged content a)

integerUpdateInner :: Mode -> ReplacementData -> Tagged Text -> Tagged Text
integerUpdateInner MathMode _rep (Tagged content NormalMode) = (Tagged content NormalMode)
integerUpdateInner MathMode rep (Tagged content MathMode) = (Tagged (commonReplacement MathMode rep content) MathMode)
integerUpdateInner NormalMode rep (Tagged content NormalMode) = (Tagged (commonReplacement NormalMode rep content) NormalMode)
integerUpdateInner NormalMode _rep (Tagged content MathMode) = (Tagged content MathMode)
integerUpdateInner _ _rep a = a

clearFormatting :: Tagged Text -> Tagged Text
clearFormatting (Tagged txt NormalMode) = Tagged (clearFormattingInner txt) NormalMode
clearFormatting (Tagged txt MathMode) = Tagged (clearFormattingInner txt) MathMode
clearFormatting a = a

-- $
-- >>> clearFormattingInner "11{,}21 11{,}2 22{,}2 2{,} 2,2 d{,}d 2\\,2 a\\,2 \\, {,} , 2\\, \\,2"
-- "11,21 11,2 22,2 2{,} 2,2 d{,}d 22 a\\,2 \\, {,} , 2\\, \\,2"
clearFormattingInner :: Text -> Text
clearFormattingInner = replaceAll spaceRep
                     . replaceAll commaRep

timeLongUpdate :: Tagged Text -> Tagged Text
timeLongUpdate (Tagged txt NormalMode) = Tagged (commonReplacement NormalMode timeLongRep txt) NormalMode
timeLongUpdate a = a

timeShortUpdate :: Tagged Text -> Tagged Text
timeShortUpdate (Tagged txt NormalMode) = Tagged (commonReplacement NormalMode timeShortRep txt) NormalMode
timeShortUpdate a = a

integer1NormalUpdate :: Tagged Text -> Tagged Text
integer1NormalUpdate = integerUpdateInner NormalMode integer1Rep

integer2NormalUpdate :: Tagged Text -> Tagged Text
integer2NormalUpdate = integerUpdateInner NormalMode integer2Rep

integer3NormalUpdate :: Tagged Text -> Tagged Text
integer3NormalUpdate = integerUpdateInner NormalMode integer3Rep

integer4NormalUpdate :: Tagged Text -> Tagged Text
integer4NormalUpdate = integerUpdateInner NormalMode integer4Rep

integer5NormalUpdate :: Tagged Text -> Tagged Text
integer5NormalUpdate = integerUpdateInner NormalMode integer5Rep

integer2MathUpdate :: Tagged Text -> Tagged Text
integer2MathUpdate = integerUpdateInner MathMode integer2Rep

integer3MathUpdate :: Tagged Text -> Tagged Text
integer3MathUpdate = integerUpdateInner MathMode integer3Rep

integer4MathUpdate :: Tagged Text -> Tagged Text
integer4MathUpdate = integerUpdateInner MathMode integer4Rep

integer5MathUpdate :: Tagged Text -> Tagged Text
integer5MathUpdate = integerUpdateInner MathMode integer5Rep

-- Assuming there is practically never $\textit{}$.
-- replace by @mathModeDictionary regexps@. All text in math mode inside.
mathItalicUpdate :: Tagged Text -> Tagged Text
mathItalicUpdate = mathSpecialUpdate Italic

mathBoldUpdate :: Tagged Text -> Tagged Text
mathBoldUpdate = mathSpecialUpdate Bold
