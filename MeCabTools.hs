module MeCabTools
( parseChasenFormat
, parseFilteredChasenFormat
, parseWakati
)where

import Text.Regex
import Text.MeCab
import Data.List
import Debug.Trace

parseChasenFormat :: String -> IO String
parseFilteredChasenFormat :: String -> [String] -> IO [String]
parseWakati :: String -> IO String

filterByPartOfSpeech :: String -> [String] -> [String]
filterAPart :: String -> [String] -> Bool
doEachFilters :: String -> String -> Bool
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

filterAPart line filters = or $ map (\a -> doEachFilters (head (splitOnConma ((splitOnTab line) !! 1))) a) filters

filterByPartOfSpeech chasenString filters = [head (splitOnTab x) | x <- (lines chasenString), (filterAPart x filters) == True]
