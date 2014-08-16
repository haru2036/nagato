{-# LANGUAGE DeriveDataTypeable #-}
module Text.Nagato.Train
( freqsToProbs
  ,trainClass
  ,countClass
  ,parseAndCountClass 
  ,parseAndTrainClass 
)where
import Data.List as L
import Data.Map
import Text.Nagato.Models
import Text.Nagato.MeCabTools as MeCabTools

searchAndCountWords :: String -> [String] -> Int
searchAndCountWords key items = length $ L.filter (==key) items

getUnigramFrequency :: [String] -> Freqs
getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

parseString :: String -> IO [String]
parseString doc = MeCabTools.parseFilteredChasenFormat doc ["名詞"]

countClass :: [String] -> Freqs
countClass = getUnigramFrequency 

trainClass :: [String] -> Probs
trainClass doc = (freqsToProbs . countClass) doc 2 

freqsToProbs :: Freqs -> Int -> Probs
freqsToProbs classMap alpha = Data.Map.map (\a -> ((realToFrac (a + 1) / (realToFrac ((sum (elems classMap) + (length (keys classMap)) * (alpha - 1))))))) classMap

parseAndCountClass :: String -> IO Freqs
parseAndCountClass docStr = do
  parsed <- parseString docStr
  return $ countClass parsed

parseAndTrainClass :: String -> IO Probs
parseAndTrainClass docStr = do
  parsed <- parseString docStr
  return $ trainClass parsed

