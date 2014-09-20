module Text.Nagato.MeCabTools
( parseFormat
, parseFilteredChasenFormat
, parseWakati
, splitOnConma
, splitOnTab
)where

import Text.Regex
import Text.MeCab
import Data.List

parseWakati :: String -> IO String
parseWakati sentence = do
  mecab <- new2 "-Owakati"
  parse mecab sentence

parseFormat :: String -> IO String
parseFormat sentence = do
  mecab <- new2 ""
  parse mecab sentence

parseFilteredChasenFormat :: String -> [String] -> IO [String]
parseFilteredChasenFormat sentence filters = do
  string <- parseFormat sentence
  let endStripped = init $ lines string
  let strippedChasenString = unlines [x | x <- endStripped, x /= ""]
  let returned = filterByPartOfSpeech strippedChasenString filters
  return returned

splitOnTab :: String -> [String]
splitOnTab line = splitRegex (mkRegex "\t") line

splitOnConma :: String -> [String]
splitOnConma line = splitRegex (mkRegex ",") line

doEachFilters :: String -> String -> Bool
doEachFilters part filtersItem = isPrefixOf part filtersItem

filterAPart :: String -> [String] -> Bool
filterAPart line filters = or $ map (\a -> doEachFilters (splitLine line) a) filters

getSpeechFromSplitTab :: String -> String
getSpeechFromSplitTab line = let splitted = splitOnTab line
  in if (length splitted) >= 2
    then splitted !! 1
    else ""

splitLine :: String -> String 
splitLine line = let splitted = splitOnConma (getSpeechFromSplitTab line)
  in if splitted /= []
    then head splitted
    else ""

filterByPartOfSpeech :: String -> [String] -> [String]
filterByPartOfSpeech string filters = [head (splitOnTab x) | x <- (lines string), (filterAPart x filters) == True]
