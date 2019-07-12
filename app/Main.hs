module Main where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Turtle

import qualified Control.Foldl as Fold
import qualified Filesystem.Path as Path

import Data.Text (Text)
import qualified Data.Text as T

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

updateFileData :: Dictionary -> Trimmed -> Trimmed
updateFileData dict (Trimmed h body t) = Trimmed h new_body t
  where
    -- Tagged Text -> [Tagged Text] -> [Tagged Text] -> [[Tagged Text]] -> [Tagged Text] -> [Tagged Text] -> Tagged Text -> Text
    new_body = taggedBody . genConcat $ integer1Update
           <$> (genConcat $ markMathMode mathModeDictionary . integer2RevertUpdate . integer2Update
           <$> (genConcat $ markMathMode mathModeDictionary . integer3RevertUpdate . integer3Update
           <$> (genConcat $ markMathMode mathModeDictionary . integer4RevertUpdate . integer4Update
           <$> (genConcat $ markMathMode mathModeDictionary . integer5RevertUpdate . integer5Update
           <$> (genConcat $ markMathMode mathModeDictionary . timeUpdate
           <$> (genConcat $ markMathMode mathModeDictionary . fractionalRevertUpdate . fractionalUpdate
           <$> (genConcat $ markCommands dict <$> markMathMode mathModeDictionary (Tagged body NormalMode))))))))
    
run :: FilePath -> FilePath -> IO ()
run path dictPath = do
  content <- trimEnds . map lineToText <$> fold (input path) Fold.list
  dictionary <- Dictionary <$> map (readRegex . lineToText) <$> fold (input dictPath) Fold.list
  outputTrimmed (Path.replaceExtension path outputExtension) (updateFileData dictionary content)
  
main :: IO ()
main = do
  Opts{..} <- options "Input directory" parser
  files <- fold (find (suffix ".tex") optsDirectory) Fold.list
  mapM_ (flip run optsDictionary) files