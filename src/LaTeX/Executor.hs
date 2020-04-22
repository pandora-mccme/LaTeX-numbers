{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Executor where

import Data.Text (Text)
import qualified Data.Text as T

import LaTeX.Types
import LaTeX.Demarkation
import LaTeX.Replacement.Procedures
import LaTeX.Replacement.Rules (unaryMinus, multiSpaces, multiSpacesTilde)

-- $setup
-- >>> :set -XOverloadedStrings

boldApply :: Tagged Text -> Tagged Text
boldApply = foldMap mathBoldUpdate . markBold

italicApply :: Tagged Text -> Tagged Text
italicApply = foldMap mathItalicUpdate . markItalic

includeUnaryMinus :: Tagged Text -> Tagged Text
includeUnaryMinus (Tagged txt mode) = Tagged (replaceAll unaryMinus txt) mode

replaceMultiSpaces :: Tagged Text -> Tagged Text
replaceMultiSpaces (Tagged txt mode) = Tagged (replaceAll multiSpaces txt) mode

replaceMultiSpacesTilde :: Tagged Text -> Tagged Text
replaceMultiSpacesTilde (Tagged txt mode) = Tagged (replaceAll multiSpacesTilde txt) mode

-- |
-- >>> replaceAll unaryMinus "-$12.3$"
-- "$-12.3$"
-- >>> replaceAll unaryMinus "$-$"
-- "$-$"
-- >>> replaceAll multiSpaces "text   with  multiple spaces"
-- "text with multiple spaces"
-- >>> replaceAll multiSpacesTilde "text ~with~ multiple spaces"
-- "text~with~multiple spaces"
commonProcedures :: Tagged Text -> Tagged Text
commonProcedures = replaceMultiSpaces
                 . replaceMultiSpacesTilde
                 . includeUnaryMinus

-- Warning: not associative operation.
mathApply :: Dictionary -> Dictionary -> Tagged Text -> Tagged Text
mathApply dict mathDict = foldl1 (<>)
                        . foldl1 (\f g x -> f x >>= g) actions
  where
    actions = [ markCommands dict
              , markMathModeExt mathDict
              , markMathMode . fractionalMathUpdate . fractionalNormalUpdate . clearCyrillic . clearFormatting
              , markMathMode . timeUpdate
              , return . commonProcedures . integerMathUpdate . integerNormalUpdate . placeholderMathUpdate . placeholderNormalUpdate
              ]

executeCorrector :: Rack -> Trimmed -> Trimmed
executeCorrector Rack{..} (Trimmed h body t) = Trimmed h new_body t
  where
    new_body = taggedBody
             . tildeUpdate dictTildes
             . italicApply
             . boldApply
             . mathApply dictCMD dictMath
             $ (Tagged body NormalMode)
