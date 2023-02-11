{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Data.Maybe (fromMaybe)
import Data.Tuple.Curry
import Turtle hiding (stdout, stderr)

import qualified Control.Foldl as Fold
import qualified Filesystem.Path as Path

import LaTeX.Types
import LaTeX.Options
import LaTeX.Utils
import LaTeX.Load
import LaTeX.Executor (executeCorrector)

import LaTeX.Entry (readRulesExplicit, readConfig)

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fileName (Trimmed h b t) = writeTextFile fileName $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t

run :: Bool -> Rack -> FilePath -> IO ()
run debug rack path = do
  mcontent <- fold (input path) Fold.list
          >>= return . trimEnds . map lineToText
  let filePath = if debug
        then Path.replaceExtension path outputExtensionDebug
        else path
  case mcontent of
    Just content -> do
      let fixedContent = content {
          trimmedBody = replaceProblemTags 1 (trimmedBody content) (encodeString (filename path))
        }
      outputTrimmed filePath
        (executeCorrector rack fixedContent)
    Nothing -> return ()

pprint :: Dictionary -> IO ()
pprint (Dictionary dict) = sequence_ . map print $ dict

prettyPrint :: Rack -> IO ()
prettyPrint (Rack c m _t) = putStrLn "Commands dictionary:"
                         >> pprint c
                         >> putStrLn "\nMath mode dictionary:"
                         >> pprint m

main :: IO ()
main = do
  opts@Opts{..} <- options "Fix number formatting through directory." parser
  rack <- (readConfig (fromMaybe "LaTeX-numbers.ini" optsConfig) >>= uncurryN readRulesExplicit)

  if optsDebug
    then prettyPrint rack
    else return ()

  if optsSingleFile
    then run optsDebug rack optsDirectory
    else do
      files <- fold (find (suffix ".tex") optsDirectory) Fold.list
      mapM_ (run optsDebug rack) files
