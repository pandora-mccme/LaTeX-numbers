module LaTeX.Replacement.Procedures where

import Data.Monoid ((<>))

import Data.Text (Text)
import Data.Text.ICU.Replace (replaceAll)

import LaTeX.Replacement.Rules
import LaTeX.Types

toMathMode :: ReplacementData -> ReplacementData
toMathMode Replacement{..} = Replacement replacementPattern ("$$" <> replacementResult <> "$$")

toItalic :: ReplacementData -> ReplacementData
toItalic Replacement{..} = Replacement replacementPattern ("$$ \\mathit{" <> replacementResult <> "} $$")

toBold :: ReplacementData -> ReplacementData
toBold Replacement{..} = Replacement replacementPattern ("$$ \\mathbf{" <> replacementResult <> "} $$")

replaceAll_ :: ReplacementData -> Text -> Text
replaceAll_ Replacement{..} = replaceAll replacementPattern replacementResult

modifier :: Mode -> ReplacementData -> ReplacementData
modifier MathMode = id
modifier NormalMode = toMathMode
modifier CMD = id
modifier Italic = toItalic
modifier Bold = toBold

mathSpecialReplacement :: Mode -> Text -> Text
mathSpecialReplacement mode =
    replaceAll_ (modifier mode mathBracketsRep)
  . replaceAll_ (modifier mode mathDollarsRep)

integerReplacement :: Mode -> ReplacementData -> Text -> Text
integerReplacement mode rep = replaceAll_ (modifier mode rep)

-- FIXME: rewrite with fold.
fractionalReplacement :: Mode -> Text -> Text
fractionalReplacement mode =
    replaceAll_ (modifier mode $ fractional1_1Rep False)
  . replaceAll_ (modifier mode $ fractional2_1Rep False)
  . replaceAll_ (modifier mode $ fractional1_2Rep False)
  . replaceAll_ (modifier mode $ fractional1_3Rep False)
  . replaceAll_ (modifier mode $ fractional3_1Rep False)
  . replaceAll_ (modifier mode $ fractional2_2Rep False)
  . replaceAll_ (modifier mode $ fractional3_2Rep False)
  . replaceAll_ (modifier mode $ fractional2_3Rep False)
  . replaceAll_ (modifier mode $ fractional3_3Rep False)
  . replaceAll_ (modifier mode $ fractional1_1Rep True)
  . replaceAll_ (modifier mode $ fractional2_1Rep True)
  . replaceAll_ (modifier mode $ fractional1_2Rep True)
  . replaceAll_ (modifier mode $ fractional1_3Rep True)
  . replaceAll_ (modifier mode $ fractional3_1Rep True)
  . replaceAll_ (modifier mode $ fractional2_2Rep True)
  . replaceAll_ (modifier mode $ fractional3_2Rep True)
  . replaceAll_ (modifier mode $ fractional2_3Rep True)
  . replaceAll_ (modifier mode $ fractional3_3Rep True)

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
integerUpdateInner MathMode rep (Tagged content MathMode) = (Tagged (integerReplacement MathMode rep content) MathMode)
integerUpdateInner NormalMode rep (Tagged content NormalMode) = (Tagged (integerReplacement NormalMode rep content) NormalMode)
integerUpdateInner NormalMode _rep (Tagged content MathMode) = (Tagged content MathMode)
integerUpdateInner _ _rep a = a

clearFormatting :: Tagged Text -> Tagged Text
clearFormatting (Tagged txt NormalMode) = Tagged (clearFormattingInner txt) NormalMode
clearFormatting (Tagged txt MathMode) = Tagged (clearFormattingInner txt) MathMode
clearFormatting a = a

clearFormattingInner :: Text -> Text
clearFormattingInner = replaceAll_ spaceRep
                     . replaceAll_ commaRep

timeUpdate :: Tagged Text -> Tagged Text
timeUpdate (Tagged txt NormalMode) = Tagged (replaceAll_ (toMathMode timeRep) txt) NormalMode
timeUpdate a = a

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
