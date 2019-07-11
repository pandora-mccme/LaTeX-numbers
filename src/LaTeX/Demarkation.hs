{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Demarkation where

import Data.Monoid ((<>))

import Data.Text (Text)
import qualified Data.Text as T

import Data.List.Split (splitWhen)

import LaTeX.Types

trimEnds :: [Text] -> Trimmed
trimEnds content = Trimmed h b t
  where
    [h,b,t] = map (T.intercalate "\n") $ splitWhen isBeginEnd content

isBeginEnd :: Text -> Bool
isBeginEnd a =
     "\\begin{document}" `T.isInfixOf` a
  || "\\end{document}" `T.isInfixOf` a

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
