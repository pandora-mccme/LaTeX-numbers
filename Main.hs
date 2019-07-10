-- Fix number processing in LaTeX files.

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold

import qualified Filesystem.Path as Path

import Data.Monoid
import Data.List.Split
import Data.Text (Text)
import qualified Data.Text as T

-- = Data structures with helpers. All headed sections could easily be in separate modules.

data Trimmed = Trimmed {
    trimmedHead :: Text
  , trimmedBody :: Text
  , trimmedTail :: Text
  }

data Tagged a = Tagged {
    taggedBody :: a
  , taggedTag  :: Bool
  }

instance Monoid a => Monoid (Tagged a) where
  mempty = Tagged mempty True
  mappend (Tagged a fa) (Tagged b fb) = Tagged (a <> b) (fa || fb) 

genConcat :: Monoid a => [a] -> a
genConcat (a:tail) = a <> genConcat tail

outputExtension :: Text
outputExtension = "test"

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fp (Trimmed h b t) = writeTextFile fp $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t

parser :: Parser FilePath
parser = argPath "path" "Directory with LaTeX to fix"

-- = Ends trimmer.

trimEnds :: [Text] -> Trimmed
trimEnds content = Trimmed h b t
  where
    [h,b,t] = map (T.intercalate "\n") $ splitWhen isBeginEnd content

isBeginEnd :: Text -> Bool
isBeginEnd a =
     "\\begin{document}" `T.isInfixOf` a
  || "\\end{document}" `T.isInfixOf` a

-- = Classifier.

-- Tagged with @False@ for text in dollars.
splitClassify :: Tagged Text -> [Tagged Text]
splitClassify = undefined

-- = Regexp replacer.

fractionalUpdate :: Tagged Text -> Tagged Text
fractionalUpdate = undefined

integerUpdate :: Tagged Text -> Tagged Text
integerUpdate = undefined

-- = Application.

updateFileData :: Trimmed -> Trimmed
updateFileData (Trimmed h body t) = Trimmed h new_body t
  where
    -- [Tagged Text] -> [Tagged Text] -> [Tagged Text] -> [[Tagged Text]] -> [Tagged Text] -> [Tagged Text] -> Tagged Text -> Text
    new_body = taggedBody . genConcat $ integerUpdate <$> genConcat (splitClassify . fractionalUpdate <$> splitClassify (Tagged body True))
    
run :: FilePath -> IO ()
run path = do
  content <- trimEnds . map lineToText <$> fold (input path) Fold.list
  outputTrimmed (Path.replaceExtension path outputExtension) (updateFileData content)

-- trimEnds >> splitClassify >> fractionalsProcess >> concat . splitClassify >> integersClassify >> concat.
-- splitClassify: automaton to get through content (as string - bottleneck) and set data in dollars to another state (keeping dollars in them).
--   String -> [(String, Bool)] == [markedString]. If there is nothing better.
-- fractionalProcess - regexp search and replace. Do some clever regular expression and match through all fractionals regarding spacing inside number.
-- Put in dollars. [markedString] -> [markedString]
-- concat . splitClassify - redo splitClassify for all processed blocks, concat result in one list again.
-- integersClassify - Go through left out integers.
-- Concat again, build up to Text.

-- Go with tildePrepositions - singular regexp.

-- Caveats: numbers in tables. To be checked in regexp. Probably exclude \{\a*\d+\a*\}

main :: IO ()
main = do
  basePath <- options "Input directory" parser
  files <- fold (find (suffix ".tex") basePath) Fold.list
  mapM_ run files
