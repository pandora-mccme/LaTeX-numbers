{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Types where

import Data.Monoid
import Data.Semigroup

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.ICU (Regex)

newtype Dictionary = Dictionary [Regex] deriving (Show, Semigroup, Monoid)

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

instance Semigroup a => Semigroup (Tagged a) where
  (<>) (Tagged a fa) (Tagged b fb) = Tagged (a Data.Semigroup.<> b) (fa `max` fb)

instance Monoid a => Monoid (Tagged a) where
  mempty = Tagged mempty NormalMode
