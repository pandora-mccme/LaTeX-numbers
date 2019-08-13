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
mathApply dict mathDict = foldMap (integerMathUpdate . integerNormalUpdate)
                        . foldl1 (\f g x -> f x >>= g) actions
  where
    actions = [ markMathModeExt mathDict
              , markCommands dict
              , return . clearFormatting
              , markMathMode . fractionalMathUpdate . fractionalNormalUpdate
              , markMathMode . timeUpdate
              ]

executeCorrector :: Dictionary -> Dictionary -> Trimmed -> Trimmed
executeCorrector dict mathDict (Trimmed h body t) = Trimmed h new_body t
  where
    new_body = taggedBody
             . italicApply
             . boldApply
             . mathApply dict mathDict
             $ (Tagged body NormalMode)
