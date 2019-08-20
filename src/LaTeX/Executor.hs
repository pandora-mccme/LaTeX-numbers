{-# LANGUAGE RecordWildCards #-}
module LaTeX.Executor where

import Data.Text (Text)

import LaTeX.Types
import LaTeX.Demarkation
import LaTeX.Replacement.Procedures

boldApply :: Tagged Text -> Tagged Text
boldApply = foldMap mathBoldUpdate . markBold

italicApply :: Tagged Text -> Tagged Text
italicApply = foldMap mathItalicUpdate . markItalic

-- Warning: not associative operation.
mathApply :: Dictionary -> Dictionary -> Tagged Text -> Tagged Text
mathApply dict mathDict = foldl1 (<>)
                        . foldl1 (\f g x -> f x >>= g) actions
  where
    actions = [ markCommands dict
              , markMathModeExt mathDict
              , return . clearFormatting
              , markMathMode . fractionalMathUpdate . fractionalNormalUpdate
              , markMathMode . timeUpdate
              , return . integerMathUpdate . integerNormalUpdate
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
