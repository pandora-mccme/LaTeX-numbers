{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Options where

import Prelude hiding (FilePath)
import Turtle (FilePath, optional)
import Turtle.Options

data Opts = Opts {
    optsConfig               :: Maybe FilePath
  , optsDirectory            :: FilePath
  , optsDebug                :: Bool
  , optsSingleFile           :: Bool
  }

parser :: Parser Opts
parser = Opts
     <$> optional (optPath "conf" 'c' "Configuration file.")
     <*> argPath "path" "Directory with LaTeX to fix."
     <*> switch  "debug" 'D' "Write changes to another file (debug mode)."
     <*> switch  "single-file" 'S' "Single file operation mode."
