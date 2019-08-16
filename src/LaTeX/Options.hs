{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Options where

import Prelude hiding (FilePath)
import Turtle (FilePath, optional)
import Turtle.Options

data Opts = Opts {
    optsDictionary           :: FilePath
  , optsMathDictionary       :: Maybe FilePath
  , optsTildeLeftDictionary  :: Maybe FilePath
  , optsTildeRightDictionary :: Maybe FilePath
  , optsDirectory            :: FilePath
  , optsDebug                :: Bool
  , optsRegex                :: Bool
  }

parser :: Parser Opts
parser = Opts
     <$> optPath "dict" 'd' "File with list of expressions not to change numbers in."
     <*> optional (optPath "math" 'm' "Nontrivial commands enabling math mode as side effect for text inside.")
     <*> optional (optPath "tilde-left" 'l' "Dictionary with words to be prefixed with inextensible space instead of regular.")
     <*> optional (optPath "tilde-right" 'r' "Dictionary with words to be suffixed with inextensible space instead of regular.")
     <*> argPath "path" "Directory with LaTeX to fix."
     <*> switch  "debug" 'D' "Write changes to another file (debug mode)."
     <*> switch  "regex" 'R' "Read dictionary entries as plain regular expressions."
