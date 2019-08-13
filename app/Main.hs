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
import LaTeX.Utils
import LaTeX.Load
import LaTeX.Executor

data Opts = Opts {
    optsDictionary     :: FilePath
  , optsMathDictionary :: Maybe FilePath
  , optsDirectory      :: FilePath
  , optsDebug          :: Bool
  , optsRegex          :: Bool
  }

outputExtensionDebug :: Text
outputExtensionDebug = "test"

outputExtension :: Text
outputExtension = "tex"

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fp (Trimmed h b t) = writeTextFile fp $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t

parser :: Parser Opts
parser = Opts
     <$> optPath "dict" 'd' "File with list of expressions not to change numbers in."
     <*> optional (optPath "math" 'm' "Nontrivial commands enabling math mode as side effect for text inside.")
     <*> argPath "path" "Directory with LaTeX to fix."
     <*> switch  "debug" 'D' "Write changes to another file (debug mode)."
     <*> switch  "regex" 'R' "Read dictionary entries as plain regular expressions."

updateFileData :: Dictionary -> Dictionary -> Trimmed -> Trimmed
updateFileData dict mathDict (Trimmed h body t) = Trimmed h new_body t
  where
    new_body = taggedBody
             . italicApply
             . boldApply
             . mathApply dict mathDict
             $ (Tagged body NormalMode)
    
run :: Bool -> Dictionary -> Dictionary -> FilePath -> IO ()
run debug dict mathDict path = do
  mcontent <- fold (input path) Fold.list
          >>= return . trimEnds . map lineToText
  case mcontent of
    Just content -> outputTrimmed
                      (Path.replaceExtension path ext)
                      (updateFileData dict mathDict content)
    Nothing -> return ()
  where
    ext = if debug
      then outputExtensionDebug
      else outputExtension

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
