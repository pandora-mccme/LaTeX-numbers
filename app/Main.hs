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
    optsDictionary     :: FilePath
  , optsMathDictionary :: Maybe FilePath
  , optsDirectory      :: FilePath
  , optsDebug          :: Bool
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
     <*> switch  "debug" 'D' "Write changes to another file (debug mode)"

-- Too hard. Should be chunked to be supportable.
updateFileData :: Dictionary -> Dictionary -> Trimmed -> Trimmed
updateFileData dict mathDict (Trimmed h body t) = Trimmed h new_body t
  where
    -- Tagged Text -> [Tagged Text] -> [Tagged Text] -> [[Tagged Text]] -> [Tagged Text] -> [Tagged Text] -> Tagged Text -> Text
    new_body = taggedBody . genConcat $ mathItalicUpdate
           <$> (markItalic . genConcat $ mathBoldUpdate
           <$> (markBold . genConcat $ integer1NormalUpdate
           <$> (genConcat $ markMathMode . integer2MathUpdate . integer2NormalUpdate
           <$> (genConcat $ markMathMode . integer3MathUpdate . integer3NormalUpdate
           <$> (genConcat $ markMathMode . integer4MathUpdate . integer4NormalUpdate
           <$> (genConcat $ markMathMode . integer5MathUpdate . integer5NormalUpdate
           <$> (genConcat $ markMathMode . timeUpdate
           <$> (genConcat $ markMathMode . fractionalMathUpdate . fractionalNormalUpdate
           <$> (genConcat $ markCommands dict <$> markMathModeExt mathDict (Tagged body NormalMode))))))))))
    
run :: Bool -> Dictionary -> Dictionary -> FilePath -> IO ()
run debug dict mathDict path = do
  mcontent <- trimEnds . map lineToText <$> fold (input path) Fold.list
  case mcontent of
    Just content -> outputTrimmed (Path.replaceExtension path ext) (updateFileData dict mathDict content)
    Nothing -> return ()
  where
    ext = if debug
      then outputExtensionDebug
      else outputExtension
  
main :: IO ()
main = do
  Opts{..} <- options "Fix number formatting through directory." parser

  dictionary <- Dictionary 
            <$> map readRegex . filter (\l -> (not $ T.isPrefixOf "-- " l) && (l /= "")) . map lineToText
            <$> fold (input optsDictionary) Fold.list

  mathDictionary <- case optsMathDictionary of
    Just mdp -> Dictionary
            <$> map readRegex . filter (\l -> (not $ T.isPrefixOf "-- " l) && (l /= "")) . map lineToText
            <$> fold (input mdp) Fold.list
    Nothing -> return $ Dictionary []

  if optsDebug
    then print dictionary >> print mathDictionary
    else return ()

  files <- fold (find (suffix ".tex") optsDirectory) Fold.list
  mapM_ (run optsDebug dictionary mathDictionary) files
