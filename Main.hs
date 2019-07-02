-- Fix number processing in LaTeX files.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import Data.Text (Text)
import qualified Data.Text as T

parser :: Parser FilePath
parser = argPath "path" "Directory with LaTeX to fix"

updateNumbers :: FilePath -> IO ()
updateNumbers path = print path

main :: IO ()
main = do
  basePath <- options "Input directory" parser
  files <- fold (find (suffix ".tex") basePath) Fold.list
  mapM_ updateNumbers files
  
