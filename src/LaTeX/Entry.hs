{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module LaTeX.Entry where

-- Imported as to deal with filesystem instead of directory package.
import Prelude hiding (FilePath)
import Turtle hiding (stdout, stderr, x, d, e)

import Data.Tuple.Curry

import qualified Control.Foldl as Fold
import qualified Filesystem.Path.CurrentOS as Path

import qualified Data.Text as T
import Data.Text (Text)

import LaTeX.Types
import LaTeX.Utils
import LaTeX.Load

-- Library entry points.

mapEitherIO :: Show a => [Either a b] -> IO [b]
mapEitherIO [] = return []
mapEitherIO (Left str:xs) = print str
                         >> mapEitherIO xs
mapEitherIO (Right x:xs) = mapEitherIO xs
                       >>= return . (x:)

readDictionary :: (Text -> Text) -> Bool -> FilePath -> IO Dictionary
readDictionary modifier defaultRegex path = fold (input path) Fold.list
                                        >>= mapEitherIO . rawDict
                                        >>= return . Dictionary
  where
    rawDict raw = map (\p -> readRegex (isRegexp p || defaultRegex) p)
                . map (modifier . T.strip)
                . filter isPattern
                . map lineToText
                $ raw

readConfig :: FilePath -> IO (FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath, Maybe FilePath)
readConfig path = fold (input path) Fold.list >>= return . adjustTypes . map (Path.fromText . lineToText)
  where
    -- FIXME
    adjustTypes [] = error "Invalid LaTeX-numbers call: main dictionary must be provided."
    adjustTypes [a] = (a, Nothing, Nothing, Nothing, Nothing)
    adjustTypes [a,b] = (a, Just b, Nothing, Nothing, Nothing)
    adjustTypes [a,b,c] = (a, Just b, Just c, Nothing, Nothing)
    adjustTypes [a,b,c,d] = (a, Just b, Just c, Just d, Nothing)
    adjustTypes [a,b,c,d,e] = (a, Just b, Just c, Just d, Just e)
    adjustTypes xs = adjustTypes (take 5 xs)

readRules :: IO Rack
readRules = readConfig "LaTeX-numbers.ini" >>= uncurryN readRulesExplicit

readRulesExplicit :: FilePath -> Maybe FilePath
                  -> Maybe FilePath -> Maybe FilePath -> Maybe FilePath
                  -> IO Rack
readRulesExplicit mainD mathD leftD midD rightD = do
  dictCMD <- readDictionary id False mainD

  dictMath <- case mathD of
    Just mdp -> readDictionary id False mdp
    Nothing -> return $ Dictionary []

  tildeLeftDictionary <- case leftD of
    Just tldp -> readDictionary makeTildeLeftPattern True tldp
    Nothing -> return $ Dictionary []

  tildeRightDictionary <- case rightD of
    Just trdp -> readDictionary makeTildeRightPattern True trdp
    Nothing -> return $ Dictionary []

  tildeMidDictionary <- case midD of
    Just tmdp -> readDictionary makeTildeMidPattern True tmdp
    Nothing -> return $ Dictionary []

  let dictTildes = tildeLeftDictionary <> tildeRightDictionary <> tildeMidDictionary
  return Rack{..}
