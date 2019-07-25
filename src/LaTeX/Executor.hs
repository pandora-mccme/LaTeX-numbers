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
mathApply dict mathDict = foldMap integer1NormalUpdate . foldl1 (\f g x -> f x >>= g) actions
  where
    actions = [ markMathModeExt mathDict
              , markCommands dict
              -- FIXME. Should be composed in another way.
              , return . clearFormatting
              , markMathMode . fractionalMathUpdate . fractionalNormalUpdate
              , markMathMode . timeShortUpdate
              , markMathMode . timeLongUpdate
              , markMathMode . integer5MathUpdate . integer5NormalUpdate
              , markMathMode . integer4MathUpdate . integer4NormalUpdate
              , markMathMode . integer3MathUpdate . integer3NormalUpdate
              , markMathMode . integer2MathUpdate . integer2NormalUpdate
              ]
