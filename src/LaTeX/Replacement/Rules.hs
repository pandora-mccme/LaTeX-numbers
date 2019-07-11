{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Replacement.Rules where

import Data.Text.ICU (Regex)
import Data.Text.ICU.Replace (Replace)

type Malformed = Bool

data ReplacementData = Replacement {
    replacementPattern :: Regex
  , replacementResult  :: Replace
  }

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
