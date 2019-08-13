{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Turtle hiding (stdout, stderr)

import qualified Control.Foldl as Fold
import qualified Filesystem.Path as Path

import Data.Text (Text)

import LaTeX.Types
import LaTeX.Options
import LaTeX.Utils
import LaTeX.Load
import LaTeX.Executor

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fp (Trimmed h b t) = writeTextFile fp $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t
    
mapEitherIO :: Show a => [Either a b] -> IO [b]
mapEitherIO [] = return []
mapEitherIO (Left str:xs) = print str
                         >> mapEitherIO xs
mapEitherIO (Right x:xs) = mapEitherIO xs
                       >>= return . (x:)

readDictionary :: Bool -> FilePath -> IO Dictionary
readDictionary regex path = do
  dict <- fold (input path) Fold.list
      >>= mapEitherIO . rawDict
  return $ Dictionary dict
  where
    rawDict raw = map (readRegex regex)
                . filter isPattern
                . map lineToText
                $ raw

run :: Bool -> Dictionary -> Dictionary -> FilePath -> IO ()
run debug dict mathDict path = do
  mcontent <- fold (input path) Fold.list
          >>= return . trimEnds . map lineToText
  case mcontent of
    Just content -> outputTrimmed
                      (Path.replaceExtension path (chooseExtension debug))
                      (executeCorrector dict mathDict content)
    Nothing -> return ()


main :: IO ()
main = do
  Opts{..} <- options "Fix number formatting through directory." parser

  dictionary <- readDictionary optsRegex optsDictionary

  mathDictionary <- case optsMathDictionary of
    Just mdp -> readDictionary optsRegex mdp
    Nothing -> return $ Dictionary []

  if optsDebug
    then print dictionary
      >> print mathDictionary
      >> print defaultMathModeDictionary
    else return ()

  files <- fold (find (suffix ".tex") optsDirectory) Fold.list
  mapM_ (run optsDebug dictionary mathDictionary) files
