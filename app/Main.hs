{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold
import qualified Filesystem.Path as Path

import Data.Text (Text)

import LaTeX.Types
import LaTeX.Demarkation
import LaTeX.Replacement.Procedures

data Opts = Opts {
    optsDictionary :: FilePath
  , optsDirectory  :: FilePath
  }

outputExtension :: Text
outputExtension = "test"

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fp (Trimmed h b t) = writeTextFile fp $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t

parser :: Parser Opts
parser = Opts
     <$> optPath "dict" 'd' "File with list of expressions not to change numbers in."
     <*> argPath "path" "Directory with LaTeX to fix"

-- Unbounded rules (according to diff for all 38 prototypes of 1st lesson.):
-- 2. `_\d_\d_\d}` in problem header. Lenient.
-- 3. Tables: `p{0.24\textwidth}`, `p{4}`, `p{0.24cm}`, `C{4cm}`, `\cline{3-6}`, `\multicolon{2}`, `\multirow{2}`.
--    Note fixed places - numbers to be reworked can happen in other brackets and in the same line.
-- 5. `\raisebox{1.5ex}[0cm][0cm]`
-- 7. \includegraphics[width=0.7\linewidth]{cube0.jpg} Double hell. Fuck.
--    Probably there is really reason for a dictionary of used commands with listed number of arguments.
--    They must go out during regexps - split by them and move them to another state.
--    For all commands. This example clearly states it cannot be done in regexps section.
--
-- Demarkation:
-- Solution 2: Dictionary (of regexps) reading and subtagging while regexping - all odd indices, supposedly.
--             A bit hard, but not very -- 20-30 additional lines. Solves 2, 3, 5, 7. Problem is with split by regexp operation.

updateFileData :: Trimmed -> Trimmed
updateFileData (Trimmed h body t) = Trimmed h new_body t
  where
    -- Tagged Text -> [Tagged Text] -> [Tagged Text] -> [[Tagged Text]] -> [Tagged Text] -> [Tagged Text] -> Tagged Text -> Text
    new_body = taggedBody . genConcat $ integer1Update
           <$> (genConcat $ splitClassify . integer2RevertUpdate . integer2Update
           <$> (genConcat $ splitClassify . integer3RevertUpdate . integer3Update
           <$> (genConcat $ splitClassify . integer4RevertUpdate . integer4Update
           <$> (genConcat $ splitClassify . integer5RevertUpdate . integer5Update
           <$> (genConcat $ splitClassify . fractionalRevertUpdate . fractionalUpdate
           <$> splitClassify (Tagged body NormalMode))))))
    
run :: FilePath -> IO ()
run path = do
  content <- trimEnds . map lineToText <$> fold (input path) Fold.list
  outputTrimmed (Path.replaceExtension path outputExtension) (updateFileData content)

main :: IO ()
main = do
  Opts{..} <- options "Input directory" parser
  files <- fold (find (suffix ".tex") optsDirectory) Fold.list
  mapM_ run files
