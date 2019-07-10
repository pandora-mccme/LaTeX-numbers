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
import Data.Text.ICU.Replace

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
genConcat [] = mempty
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
splitClassify (Tagged a _) = map (\(Tagged ar br) -> Tagged (T.pack ar) br) result
  where
    result = splitClassifyInternal (T.unpack a) (Tagged "" True)

splitClassifyInternal :: String -> Tagged String -> [Tagged String]

splitClassifyInternal [] (Tagged a False) = error "Imbalanced dollars in file."

splitClassifyInternal [] (Tagged a True) = [Tagged a True]

splitClassifyInternal ('$':xs) (Tagged a False) = (Tagged (a <> "$") False) : splitClassifyInternal xs (Tagged "" True)

splitClassifyInternal ('$':xs) (Tagged a True) = (Tagged a True) : splitClassifyInternal xs (Tagged "$" False)

-- Assuming it's unreasonable to call mathmode inside mathmode.
splitClassifyInternal ('\\':')':xs) (Tagged a False) = (Tagged (a <> "\\)") False) : splitClassifyInternal xs (Tagged "" True)

splitClassifyInternal ('\\':'(':xs) (Tagged a True) = (Tagged a True) : splitClassifyInternal xs (Tagged "\\(" False)

splitClassifyInternal ('\\':[]) (Tagged a True) = error "Hanging escape character at the end of the file."

splitClassifyInternal ( x :xs) (Tagged a b) = splitClassifyInternal xs (Tagged (a <> [x]) b)

-- = Regexp replacer.

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

-- FIXME: dog-nail. I cannot split matched text into chunks via regexp. So I assume (reasonably) we have small number length at both sides.
-- <FUCK MYSELF>

-- Make newtype.
type ReplacementData = (Replace, Text)  -- TODO: Rework all @Update@ functions. Move regexps with names to separate objects, make one function and aliases.
-- Just a bit more focused.

fractionalUpdate :: Tagged Text -> Tagged Text
fractionalUpdate (Tagged content False) = (Tagged content False)
fractionalUpdate (Tagged content True) = (Tagged updated_content True)
  where 
    -- Fractionals (make almost nice normal)
    f0 = replaceAll "(\\d)\\{,\\}(\\d)" "$1,$2" content
    -- Fractionals (mistyped)
    f01 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3{,}$4\\,$5\\,$6$$" f0
    f02 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2{,}$3\\,$4\\,$5$$" f01
    f03 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3{,}$4\\,$5$$" f02
    f04 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1\\,$2{,}$3\\,$4$$" f03
    f05 = replaceAll "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1{,}$2\\,$3$$" f04
    f06 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(?![$\\d])" "$$$1\\,$2{,}$3$$" f05
    f07 = replaceAll "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(?![$\\d])" "$$$1{,}$2$$" f06
    -- Fractionals
    f1 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3{,}$4\\,$5\\,$6$$" f07
    f2 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2{,}$3\\,$4\\,$5$$" f1
    f3 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3{,}$4\\,$5$$" f2
    f4 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1\\,$2{,}$3\\,$4$$" f3
    f5 = replaceAll "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1{,}$2\\,$3$$" f4
    f6 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(?![$\\d])" "$$$1\\,$2{,}$3$$" f5
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(?![$\\d])" "$$$1{,}$2$$" f6

-- Why five? Because there is strong dollar duplication. Fuck regexp.
integer1Update :: Tagged Text -> Tagged Text
integer1Update (Tagged content False) = (Tagged content False)
integer1Update (Tagged content True) = (Tagged updated_content True)
  where
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(?![$\\d])" "$$$1$$" content

integer2Update :: Tagged Text -> Tagged Text
integer2Update (Tagged content False) = (Tagged content False)
integer2Update (Tagged content True) = (Tagged updated_content True)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(?![$\\d])" "$$$1\\,$2$$" content

integer3Update :: Tagged Text -> Tagged Text
integer3Update (Tagged content False) = (Tagged content False)
integer3Update (Tagged content True) = (Tagged updated_content True)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3$$" content

integer4Update :: Tagged Text -> Tagged Text
integer4Update (Tagged content False) = (Tagged content False)
integer4Update (Tagged content True) = (Tagged updated_content True)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3\\,$4$$" content

integer5Update :: Tagged Text -> Tagged Text
integer5Update (Tagged content False) = (Tagged content False)
integer5Update (Tagged content True) = (Tagged updated_content True)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$$$1\\,$2\\,$3\\,$4\\,$5$$" content

-- FIXME: Code duplication! True by false and dollars. This code can be a lot more compact.

fractionalRevertUpdate :: Tagged Text -> Tagged Text
fractionalRevertUpdate (Tagged content True) = (Tagged content True)
fractionalRevertUpdate (Tagged content False) = (Tagged updated_content False)
  where 
    -- Fractionals (make almost nice normal)
    f0 = replaceAll "(\\d)\\{,\\}(\\d)" "$1,$2" content
    -- Fractionals (mistyped)
    f01 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5\\,$6" f0
    f02 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4\\,$5" f01
    f03 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5" f02
    f04 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4" f03
    f05 = replaceAll "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1{,}$2\\,$3" f04
    f06 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(?![$\\d])" "$1\\,$2{,}$3" f05
    f07 = replaceAll "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(?![$\\d])" "$1{,}$2" f06
    -- Fractionals
    f1 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5\\,$6" f07
    f2 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4\\,$5" f1
    f3 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5" f2
    f4 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4" f3
    f5 = replaceAll "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1{,}$2\\,$3" f4
    f6 = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(?![$\\d])" "$1\\,$2{,}$3" f5
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(?![$\\d])" "$1{,}$2" f6

integer2RevertUpdate :: Tagged Text -> Tagged Text
integer2RevertUpdate (Tagged content True) = (Tagged content True)
integer2RevertUpdate (Tagged content False) = (Tagged updated_content False)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2" content

integer3RevertUpdate :: Tagged Text -> Tagged Text
integer3RevertUpdate (Tagged content True) = (Tagged content True)
integer3RevertUpdate (Tagged content False) = (Tagged updated_content False)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3" content

integer4RevertUpdate :: Tagged Text -> Tagged Text
integer4RevertUpdate (Tagged content True) = (Tagged content True)
integer4RevertUpdate (Tagged content False) = (Tagged updated_content False)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3\\,$4" content

integer5RevertUpdate :: Tagged Text -> Tagged Text
integer5RevertUpdate (Tagged content True) = (Tagged content True)
integer5RevertUpdate (Tagged content False) = (Tagged updated_content False)
  where 
    updated_content = replaceAll "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3\\,$4\\,$5" content

-- = Application.

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
           <$> splitClassify (Tagged body True))))))
    
run :: FilePath -> IO ()
run path = do
  content <- trimEnds . map lineToText <$> fold (input path) Fold.list
  outputTrimmed (Path.replaceExtension path outputExtension) (updateFileData content)

main :: IO ()
main = do
  basePath <- options "Input directory" parser
  files <- fold (find (suffix ".tex") basePath) Fold.list
  mapM_ run files
