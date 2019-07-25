{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Replacement.Rules where

import Data.Text.ICU (Regex)
import Data.Text.ICU.Replace (Replace)

data ReplacementData = Replacement {
    replacementPattern :: Regex
  , replacementResult  :: Replace
  }

commaRep :: ReplacementData
commaRep = Replacement "(\\d)\\{,\\}(\\d)" "$1,$2"

spaceRep :: ReplacementData
spaceRep = Replacement "(\\d)\\\\,(\\d)" "$1$2"

fractional3_3Rep :: ReplacementData
fractional3_3Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})[\\.,](\\d{1,3})~*(\\d{3})~*(\\d{3})" "$1\\,$2\\,$3{,}$4$5$6"

fractional2_3Rep :: ReplacementData
fractional2_3Rep = Replacement "(\\d{1,3})~*(\\d{3})[\\.,](\\d{1,3})~*(\\d{3})~*(\\d{3})" "$1\\,$2{,}$3$4$5"

fractional1_3Rep :: ReplacementData
fractional1_3Rep = Replacement "(\\d{1,3})[\\.,](\\d{1,3})~*(\\d{3})~*(\\d{3})" "$1{,}$3$4$5"

fractional3_2Rep :: ReplacementData
fractional3_2Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})[\\.,](\\d{1,3})~*(\\d{3})" "$1\\,$2\\,$3{,}$4$5"

fractional3_1Rep :: ReplacementData
fractional3_1Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})[\\.,](\\d{1,3})" "$1\\,$2\\,$3{,}$4"

fractional2_2Rep :: ReplacementData
fractional2_2Rep = Replacement "(\\d{1,3})~*(\\d{3})[\\.,](\\d{1,3})~*(\\d{3})" "$1\\,$2{,}$3$4"

fractional2_1Rep :: ReplacementData
fractional2_1Rep = Replacement "(\\d{1,3})[\\.,](\\d{1,3})~*(\\d{3})" "$1{,}$2$3"

fractional1_2Rep :: ReplacementData
fractional1_2Rep = Replacement "(\\d{2,3})~*(\\d{3})[\\.,](\\d{1,3})" "$1\\,$2{,}$3"

fractional1_1Rep :: ReplacementData
fractional1_1Rep = Replacement "(\\d{1,4})[\\.,](\\d{1,3})" "$1{,}$2"

timeShortRep :: ReplacementData
timeShortRep = Replacement "(\\d{1,2}):(\\d{1,2})" "$1:$2"

timeLongRep :: ReplacementData
timeLongRep = Replacement "(\\d{1,2}):(\\d{1,2}):(\\d{1,2})" "$1:$2:$3"
 
integer5Rep :: ReplacementData
integer5Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})~*(\\d{3})~*(\\d{3})" "$1\\,$2\\,$3\\,$4\\,$5"
 
integer4Rep :: ReplacementData
integer4Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})~*(\\d{3})" "$1\\,$2\\,$3\\,$4"
 
integer3Rep :: ReplacementData
integer3Rep = Replacement "(\\d{1,3})~*(\\d{3})~*(\\d{3})" "$1\\,$2\\,$3"
 
integer2Rep :: ReplacementData
integer2Rep = Replacement "(\\d{2,3})~*(\\d{3})" "$1\\,$2"
 
integer1Rep :: ReplacementData
integer1Rep = Replacement "(\\d{1,4})" "$1"

mathBracketsRep :: ReplacementData
mathBracketsRep = Replacement "\\\\\\((.*?)\\\\\\)" "$1"

mathDollarsRep :: ReplacementData
mathDollarsRep = Replacement "\\$(.*?)\\$" "$1"
