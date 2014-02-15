module MeCabTools
( parseChasenFormat
, parseFilteredChasenFormat
, parseWakati
)where

import Text.Regex
import Text.MeCab
import Data.List

parseChasenFormat :: String -> IO String
parseFilteredChasenFormat :: String -> [String] -> IO [String]
parseWakati :: String -> IO String

filterByPartOfSpeech :: String -> [String] -> [String]
filterAPart :: String -> [String] -> Bool
splitLine :: String -> String 
doEachFilters :: String -> String -> Bool
getSpeechFromSplitTab :: String -> String
splitOnTab :: String -> [String]
splitOnConma :: String -> [String]

parseWakati sentence = do
  mecab <- new2 "-Owakati"
  parse mecab sentence

parseChasenFormat sentence = do
  mecab <- new2 ""
  parse mecab sentence

parseFilteredChasenFormat sentence filters = do
  chasenString <- parseChasenFormat sentence
  let endStripped = init $ lines chasenString
  let strippedChasenString = unlines [x | x <- endStripped, x /= ""]
  let returned = filterByPartOfSpeech strippedChasenString filters
  return returned

splitOnTab line = splitRegex (mkRegex "\t") line

splitOnConma line = splitRegex (mkRegex ",") line

doEachFilters part filtersItem = isPrefixOf part filtersItem

filterAPart line filters = or $ map (\a -> doEachFilters (splitLine line) a) filters

getSpeechFromSplitTab line = let splitted = splitOnTab line
  in if (length splitted) >= 2
    then splitted !! 1
    else ""

splitLine line = let splitted = splitOnConma (getSpeechFromSplitTab line)
  in if splitted /= []
    then head splitted
    else ""

filterByPartOfSpeech chasenString filters = [head (splitOnTab x) | x <- (lines chasenString), (filterAPart x filters) == True]
