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

import Data.Text.ICU (Regex)
import Data.Text.ICU.Replace

-- = Data structures with helpers. All headed sections could easily be in separate modules.

data Trimmed = Trimmed {
    trimmedHead :: Text
  , trimmedBody :: Text
  , trimmedTail :: Text
  }

data Opts = Opts {
    optsDictionary :: FilePath
  , optsDirectory  :: FilePath
  }

data Mode = CMD | MathMode | NormalMode deriving (Enum, Ord, Eq)

type Malformed = Bool
type InsideMathMode = Bool

data Tagged a = Tagged {
    taggedBody :: a
  , taggedTag  :: Mode
  }

instance Monoid a => Monoid (Tagged a) where
  mempty = Tagged mempty NormalMode
  mappend (Tagged a fa) (Tagged b fb) = Tagged (a <> b) (fa `max` fb) 

genConcat :: Monoid a => [a] -> a
genConcat [] = mempty
genConcat (a:tail) = a <> genConcat tail

outputExtension :: Text
outputExtension = "test"

outputTrimmed :: FilePath -> Trimmed -> IO ()
outputTrimmed fp (Trimmed h b t) = writeTextFile fp $
  h <> "\n\\begin{document}\n" <> b <> "\n\\end{document}\n" <> t

parser :: Parser Opts
parser = Opts
     <$> optPath "dict" 'd' "File with list of expressions not to change numbers in."
     <*> argPath "path" "Directory with LaTeX to fix"

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

splitClassify :: Tagged Text -> [Tagged Text]
splitClassify (Tagged a _) = map (\(Tagged ar br) -> Tagged (T.pack ar) br) result
  where
    result = splitClassifyInternal (T.unpack a) (Tagged "" NormalMode)

splitClassifyInternal :: String -> Tagged String -> [Tagged String]

splitClassifyInternal [] (Tagged a MathMode) = error "Imbalanced dollars in file."

splitClassifyInternal [] (Tagged a NormalMode) = [Tagged a NormalMode]

splitClassifyInternal ('$':xs) (Tagged a MathMode) = (Tagged (a <> "$") MathMode) : splitClassifyInternal xs (Tagged "" NormalMode)

splitClassifyInternal ('$':xs) (Tagged a NormalMode) = (Tagged a NormalMode) : splitClassifyInternal xs (Tagged "$" MathMode)

-- Assuming it's unreasonable to call mathmode inside mathmode.
splitClassifyInternal ('\\':')':xs) (Tagged a MathMode) = (Tagged (a <> "\\)") MathMode) : splitClassifyInternal xs (Tagged "" NormalMode)

splitClassifyInternal ('\\':'(':xs) (Tagged a NormalMode) = (Tagged a NormalMode) : splitClassifyInternal xs (Tagged "\\(" MathMode)

splitClassifyInternal ('\\':[]) (Tagged a NormalMode) = error "Hanging escape character at the end of the file."

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

data ReplacementData = Replacement {
    replacementPattern :: Regex
  , replacementResult  :: Replace
  }

toMathMode :: ReplacementData -> ReplacementData
toMathMode Replacement{..} = Replacement replacementPattern ("$$" <> replacementResult <> "$$")

replaceAll_ :: ReplacementData -> Text -> Text
replaceAll_ Replacement{..} = replaceAll replacementPattern replacementResult

-- Regexp list:
commaRep :: ReplacementData
commaRep = Replacement "(\\d)\\{,\\}(\\d)" "$1,$2"

fractional3_3Rep :: Malformed -> ReplacementData
fractional3_3Rep True = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5\\,$6"
fractional3_3Rep False = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5\\,$6"

fractional2_3Rep :: Malformed -> ReplacementData
fractional2_3Rep True = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4\\,$5"
fractional2_3Rep False = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4\\,$5"

fractional3_2Rep :: Malformed -> ReplacementData
fractional3_2Rep True = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5"
fractional3_2Rep False = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3{,}$4\\,$5"

fractional2_2Rep :: Malformed -> ReplacementData
fractional2_2Rep True = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4"
fractional2_2Rep False = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2{,}$3\\,$4"

fractional2_1Rep :: Malformed -> ReplacementData
fractional2_1Rep True = Replacement "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(\\d{3})(?![$\\d])" "$1{,}$2\\,$3"
fractional2_1Rep False = Replacement "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(\\d{3})(?![$\\d])" "$1{,}$2\\,$3"

fractional1_2Rep :: Malformed -> ReplacementData
fractional1_2Rep True = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})\\.(\\d{1,3})(?![$\\d])" "$1\\,$2{,}$3"
fractional1_2Rep False = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3}),(\\d{1,3})(?![$\\d])" "$1\\,$2{,}$3"

fractional1_1Rep :: Malformed -> ReplacementData
fractional1_1Rep True = Replacement "(?<!$\\d)(\\d{1,3})\\.(\\d{1,3})(?![$\\d])" "$1{,}$2"
fractional1_1Rep False = Replacement "(?<!$\\d)(\\d{1,3}),(\\d{1,3})(?![$\\d])" "$1{,}$2"

integer5Rep :: ReplacementData
integer5Rep = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3\\,$4\\,$5"

integer4Rep :: ReplacementData
integer4Rep = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3\\,$4"

integer3Rep :: ReplacementData
integer3Rep = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(\\d{3})(?![$\\d])" "$1\\,$2\\,$3"

integer2Rep :: ReplacementData
integer2Rep = Replacement "(?<!$\\d)(\\d{1,3})(\\d{3})(?![$\\d])" "$1\\,$2"

integer1Rep :: ReplacementData
integer1Rep = Replacement "(?<!$\\d)(\\d{1,3})(?![$\\d])" "$1"

fractionalUpdateInner :: InsideMathMode -> Tagged Text -> Tagged Text
fractionalUpdateInner True (Tagged content CMD) = (Tagged content CMD)
fractionalUpdateInner False (Tagged content CMD) = (Tagged content CMD)
fractionalUpdateInner False (Tagged content MathMode) = (Tagged content MathMode)
fractionalUpdateInner True (Tagged content NormalMode) = (Tagged content NormalMode)
fractionalUpdateInner False (Tagged content NormalMode) = (Tagged updated_content NormalMode)
  where
    updated_content = replaceAll_ (toMathMode $ fractional1_1Rep False)
                    $ replaceAll_ (toMathMode $ fractional2_1Rep False)
                    $ replaceAll_ (toMathMode $ fractional1_2Rep False)
                    $ replaceAll_ (toMathMode $ fractional2_2Rep False)
                    $ replaceAll_ (toMathMode $ fractional3_2Rep False)
                    $ replaceAll_ (toMathMode $ fractional2_3Rep False)
                    $ replaceAll_ (toMathMode $ fractional3_3Rep False)
                    $ replaceAll_ (toMathMode $ fractional1_1Rep True)
                    $ replaceAll_ (toMathMode $ fractional2_1Rep True)
                    $ replaceAll_ (toMathMode $ fractional1_2Rep True)
                    $ replaceAll_ (toMathMode $ fractional2_2Rep True)
                    $ replaceAll_ (toMathMode $ fractional3_2Rep True)
                    $ replaceAll_ (toMathMode $ fractional2_3Rep True)
                    $ replaceAll_ (toMathMode $ fractional3_3Rep True)
                    $ replaceAll_ commaRep content
fractionalUpdateInner True (Tagged content MathMode) = (Tagged updated_content MathMode)
  where
    updated_content = replaceAll_ (fractional1_1Rep False)
                    $ replaceAll_ (fractional2_1Rep False)
                    $ replaceAll_ (fractional1_2Rep False)
                    $ replaceAll_ (fractional2_2Rep False)
                    $ replaceAll_ (fractional3_2Rep False)
                    $ replaceAll_ (fractional2_3Rep False)
                    $ replaceAll_ (fractional3_3Rep False)
                    $ replaceAll_ (fractional1_1Rep True)
                    $ replaceAll_ (fractional2_1Rep True)
                    $ replaceAll_ (fractional1_2Rep True)
                    $ replaceAll_ (fractional2_2Rep True)
                    $ replaceAll_ (fractional3_2Rep True)
                    $ replaceAll_ (fractional2_3Rep True)
                    $ replaceAll_ (fractional3_3Rep True)
                    $ replaceAll_ commaRep content

fractionalUpdate :: Tagged Text -> Tagged Text
fractionalUpdate = fractionalUpdateInner False

fractionalRevertUpdate :: Tagged Text -> Tagged Text
fractionalRevertUpdate = fractionalUpdateInner True

integerUpdateInner :: InsideMathMode -> ReplacementData -> Tagged Text -> Tagged Text
integerUpdateInner True _rep (Tagged content CMD) = (Tagged content CMD)
integerUpdateInner True _rep (Tagged content NormalMode) = (Tagged content NormalMode)
integerUpdateInner True rep (Tagged content MathMode) = (Tagged (replaceAll_ rep content) MathMode)
integerUpdateInner False _rep (Tagged content CMD) = (Tagged content CMD)
integerUpdateInner False rep (Tagged content NormalMode) = (Tagged (replaceAll_ (toMathMode rep) content) NormalMode)
integerUpdateInner False _rep (Tagged content MathMode) = (Tagged content MathMode)

integer1Update :: Tagged Text -> Tagged Text
integer1Update = integerUpdateInner False integer1Rep

integer2Update :: Tagged Text -> Tagged Text
integer2Update = integerUpdateInner False integer2Rep

integer3Update :: Tagged Text -> Tagged Text
integer3Update = integerUpdateInner False integer3Rep

integer4Update :: Tagged Text -> Tagged Text
integer4Update = integerUpdateInner False integer4Rep

integer5Update :: Tagged Text -> Tagged Text
integer5Update = integerUpdateInner False integer5Rep

integer2RevertUpdate :: Tagged Text -> Tagged Text
integer2RevertUpdate = integerUpdateInner True integer2Rep

integer3RevertUpdate :: Tagged Text -> Tagged Text
integer3RevertUpdate = integerUpdateInner True integer3Rep

integer4RevertUpdate :: Tagged Text -> Tagged Text
integer4RevertUpdate = integerUpdateInner True integer4Rep

integer5RevertUpdate :: Tagged Text -> Tagged Text
integer5RevertUpdate = integerUpdateInner True integer5Rep

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
