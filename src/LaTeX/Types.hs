{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module LaTeX.Types where

import Data.Monoid

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU (Regex)

newtype Dictionary = Dictionary [Regex] deriving (Show, Monoid)

-- No replacements in comments.
defaultCmdDictionary = Dictionary ["(%.*)"]

-- $$, \(\)
defaultMathModeDictionary = Dictionary ["(\\$.*?\\$)","(\\\\\\(.*?\\\\\\))"]

-- \textbf{}
boldDictionary = Dictionary ["(\\\\textbf\\{.*?\\})"]

-- \textit{}
italicDictionary = Dictionary ["(\\\\textit\\{.*?\\})"]

data Trimmed = Trimmed {
    trimmedHead :: Text
  , trimmedBody :: Text
  , trimmedTail :: Text
  }

data Mode = CMD | Bold | Italic | MathMode | NormalMode deriving (Enum, Ord, Eq)

data Tagged a = Tagged {
    taggedBody :: a
  , taggedTag  :: Mode
  }

instance Monoid a => Monoid (Tagged a) where
  mempty = Tagged mempty NormalMode
  mappend (Tagged a fa) (Tagged b fb) = Tagged (a <> b) (fa `max` fb) 

genConcat :: Monoid a => [a] -> a
genConcat [] = mempty
genConcat (a:tail) = a <> genConcat tail
