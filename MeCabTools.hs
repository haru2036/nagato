module Text.MeCab.MeCabTools
( parseChasenFormat
, parseFilteredChasenFormat
, parseWakati
)
import Text.Regex
import Text.MeCab

parseChasenFormat :: String -> IO String
parseFilteredChasenFormat :: String -> [String] -> IO String
parseWakati :: String -> IO String

filter :: String -> [String] -> String
splitOnTab :: String -> String

parseWakati sentence = do
  mecab <- new2 "-Owakati"
  parse mecab sentence

parseChasenFormat sentence = do
  mecab <- new2 "-Ochasen"
  parse mecab sentence

parseFilteredChasenFormat sentence filter = do
  chasenString <- parseChasenFormat sentence
  filter chasenString
splitOnTab line =

filterByPartOfSpeech chasenString filter = lines chasenString 
