module Text.MeCab.MeCabTools
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
doEachFilters :: String -> String -> Bool
splitOnTab :: String -> [String]

parseWakati sentence = do
  mecab <- new2 "-Owakati"
  parse mecab sentence

parseChasenFormat sentence = do
  mecab <- new2 "-Ochasen"
  parse mecab sentence

parseFilteredChasenFormat sentence filter = do
  chasenString <- parseChasenFormat sentence
  return $ filterByPartOfSpeech chasenString filter

splitOnTab line = splitRegex (mkRegex "\t") line

doEachFilters part filter = if (head $ splitRegex (mkRegex "-") part) == filter
                            then True
                            else False

filterAPart line filters = or $ map (\a -> doEachFilters ((splitOnTab line) !! 3) a) filters

filterByPartOfSpeech chasenString filters = [x | x <- lines chasenString, filterAPart x filters== True]
