{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LaTeX.Types where

import Data.Semigroup

import Data.Text (Text)
import qualified Data.Text as T

import Text.Regex.PCRE.Heavy

data ReplacementData = Replacement {
  replacementPattern :: Regex
, replacementResult  :: [Text] -> Text
}

newtype Dictionary = Dictionary [Regex] deriving (Show, Semigroup, Monoid)

data Rack = Rack {
  dictCMD        :: Dictionary
, dictMath       :: Dictionary
, dictTildes     :: Dictionary
} deriving Show

-- No replacements in comments.
defaultCmdDictionary = Dictionary [[re|(\%.+)|]]

-- $$, \(\)
defaultMathModeDictionary = Dictionary [[re|(\${2}(?s).+?\${2})|]
                                       ,[re|(?<!\$)(\$(?s).+?\$)(?!\$)|] 
                                       ,[re|(\\\(.+?\\\))|]
                                       ,[re|(\\\[(?s).+?\\\])|]
                                       ]

-- \textbf{}
boldDictionary = Dictionary [[re|(\\textbf\{.*?\})|]]

-- \textit{}
italicDictionary = Dictionary [[re|(\\textit\{.*?\})|]]

data Trimmed = Trimmed {
  trimmedHead :: Text
, trimmedBody :: Text
, trimmedTail :: Text
}

data Mode = CMD
          | Bold
          | Italic
          | MathMode
          | NormalMode deriving (Enum, Ord, Eq)

data Tagged a = Tagged {
  taggedBody :: a
, taggedTag  :: Mode
}

instance Semigroup a => Semigroup (Tagged a) where
  (<>) (Tagged a fa) (Tagged b fb) = Tagged (a <> b) (fa `max` fb)

instance Monoid a => Monoid (Tagged a) where
  mempty = Tagged mempty NormalMode
