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

readDictionary :: (Text -> Text) -> Bool -> FilePath -> IO Dictionary
readDictionary modifier regex path = do
  dict <- fold (input path) Fold.list
      >>= mapEitherIO . rawDict
  return $ Dictionary dict
  where
    rawDict raw = map (readRegex regex)
                . map modifier
                . filter isPattern
                . map lineToText
                $ raw


readRack :: Opts -> IO Rack
readRack Opts{..} = do
  dictCMD <- readDictionary id optsRegex optsDictionary

  dictMath <- case optsMathDictionary of
    Just mdp -> readDictionary id optsRegex mdp
    Nothing -> return $ Dictionary []

  tildeLeftDictionary <- case optsTildeLeftDictionary of
    Just tldp -> readDictionary makeTildeLeftPattern True tldp
    Nothing -> return $ Dictionary []

  tildeRightDictionary <- case optsTildeRightDictionary of
    Just trdp -> readDictionary makeTildeRightPattern True trdp
    Nothing -> return $ Dictionary []

  tildeMidDictionary <- case optsTildeMidDictionary of
    Just tmdp -> readDictionary makeTildeMidPattern True tmdp
    Nothing -> return $ Dictionary []

  let dictTildes = tildeLeftDictionary <> tildeRightDictionary <> tildeMidDictionary
  return Rack{..}


run :: Bool -> Rack -> FilePath -> IO ()
run debug rack path = do
  mcontent <- fold (input path) Fold.list
          >>= return . trimEnds . map lineToText
  case mcontent of
    Just content -> do
      let fixedContent = content {
          trimmedBody = replaceProblemTags 1 (trimmedBody content) (encodeString (filename path))
        }
      outputTrimmed
        (Path.replaceExtension path (chooseExtension debug))
        (executeCorrector rack fixedContent)
    Nothing -> return ()

pprint :: Dictionary -> IO ()
pprint (Dictionary dict) = sequence_ . map print $ dict

prettyPrint :: Rack -> IO ()
prettyPrint (Rack c m t) = putStrLn "Commands dictionary:"
                        >> pprint c
                        >> putStrLn "\nMath mode dictionary:"
                        >> pprint m

main :: IO ()
main = do
  opts@Opts{..} <- options "Fix number formatting through directory." parser

  rack <- readRack opts

  if optsDebug
    then prettyPrint rack
    else return ()

  files <- fold (find (suffix ".tex") optsDirectory) Fold.list
  mapM_ (run optsDebug rack) files
