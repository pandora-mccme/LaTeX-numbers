module LaTeX.Replacement.Procedures where

import Data.Monoid ((<>))

import Data.Text (Text)
import Data.Text.ICU.Replace (replaceAll)

import LaTeX.Replacement.Rules
import LaTeX.Types

type InsideMathMode = Bool

toMathMode :: ReplacementData -> ReplacementData
toMathMode Replacement{..} = Replacement replacementPattern ("$$" <> replacementResult <> "$$")

replaceAll_ :: ReplacementData -> Text -> Text
replaceAll_ Replacement{..} = replaceAll replacementPattern replacementResult

fractionalReplacement :: (ReplacementData -> ReplacementData) -> Text -> Text
fractionalReplacement modifier txt =
    replaceAll_ (modifier $ fractional1_1Rep False)
  $ replaceAll_ (modifier $ fractional2_1Rep False)
  $ replaceAll_ (modifier $ fractional1_2Rep False)
  $ replaceAll_ (modifier $ fractional2_2Rep False)
  $ replaceAll_ (modifier $ fractional3_2Rep False)
  $ replaceAll_ (modifier $ fractional2_3Rep False)
  $ replaceAll_ (modifier $ fractional3_3Rep False)
  $ replaceAll_ (modifier $ fractional1_1Rep True)
  $ replaceAll_ (modifier $ fractional2_1Rep True)
  $ replaceAll_ (modifier $ fractional1_2Rep True)
  $ replaceAll_ (modifier $ fractional2_2Rep True)
  $ replaceAll_ (modifier $ fractional3_2Rep True)
  $ replaceAll_ (modifier $ fractional2_3Rep True)
  $ replaceAll_ (modifier $ fractional3_3Rep True)
  $ replaceAll_ commaRep txt

fractionalUpdateInner :: InsideMathMode -> Tagged Text -> Tagged Text
fractionalUpdateInner True (Tagged content CMD) = (Tagged content CMD)
fractionalUpdateInner False (Tagged content CMD) = (Tagged content CMD)
fractionalUpdateInner False (Tagged content MathMode) = (Tagged content MathMode)
fractionalUpdateInner True (Tagged content NormalMode) = (Tagged content NormalMode)
fractionalUpdateInner False (Tagged content NormalMode) = (Tagged (fractionalReplacement toMathMode content) NormalMode)
fractionalUpdateInner True (Tagged content MathMode) = (Tagged (fractionalReplacement id content) MathMode)

fractionalUpdate :: Tagged Text -> Tagged Text
fractionalUpdate = fractionalUpdateInner False

fractionalRevertUpdate :: Tagged Text -> Tagged Text
fractionalRevertUpdate = fractionalUpdateInner True

integerUpdateInner :: InsideMathMode -> ReplacementData -> Tagged Text -> Tagged Text
integerUpdateInner True _rep (Tagged content NormalMode) = (Tagged content NormalMode)
integerUpdateInner True rep (Tagged content MathMode) = (Tagged (replaceAll_ rep content) MathMode)
integerUpdateInner _ _rep (Tagged content CMD) = (Tagged content CMD)
integerUpdateInner False rep (Tagged content NormalMode) = (Tagged (replaceAll_ (toMathMode rep) content) NormalMode)
integerUpdateInner False _rep (Tagged content MathMode) = (Tagged content MathMode)

timeUpdate :: Tagged Text -> Tagged Text
timeUpdate (Tagged txt NormalMode) = Tagged (replaceAll_ (toMathMode timeRep) txt) NormalMode
timeUpdate a = a

integer1Update :: Tagged Text -> Tagged Text
integer1Update = integerUpdateInner False integer1Rep

integer2Update :: Tagged Text -> Tagged Text
integer2Update = integerUpdateInner False integer2Rep

integer3Update :: Tagged Text -> Tagged Text
integer3Update = integerUpdateInner False integer3Rep

integer4Update :: Tagged Text -> Tagged Text
integer4Update = integerUpdateInner False integer4Rep

integer5Update :: Tagged Text -> Tagged Text
integer5Update = integerUpdateInner False integer5Rep

integer2RevertUpdate :: Tagged Text -> Tagged Text
integer2RevertUpdate = integerUpdateInner True integer2Rep

integer3RevertUpdate :: Tagged Text -> Tagged Text
integer3RevertUpdate = integerUpdateInner True integer3Rep

integer4RevertUpdate :: Tagged Text -> Tagged Text
integer4RevertUpdate = integerUpdateInner True integer4Rep

integer5RevertUpdate :: Tagged Text -> Tagged Text
integer5RevertUpdate = integerUpdateInner True integer5Rep
