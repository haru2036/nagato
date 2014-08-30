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

searchAndCountWords :: (Eq a) => a -> [a] -> Int
searchAndCountWords key items = length $ L.filter (==key) items

getUnigramFrequency :: (Ord a) => [a] -> Freqs a
getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

parseString :: String -> IO [String]
parseString doc = MeCabTools.parseFilteredChasenFormat doc ["名詞"]

countClass :: (Ord a) => [a] -> Freqs a
countClass = getUnigramFrequency 

trainClass :: (Ord a) => [a] -> Probs a
trainClass doc = (freqsToProbs . countClass) doc 2 

freqsToProbs :: Freqs a -> Int -> Probs a
freqsToProbs classMap alpha = Data.Map.map (\a -> ((realToFrac (a + 1) / (realToFrac ((sum (elems classMap) + (length (keys classMap)) * (alpha - 1))))))) classMap

parseAndCountClass :: String -> IO (Freqs String)
parseAndCountClass docStr = do
  parsed <- parseString docStr
  return $ countClass parsed

parseAndTrainClass :: String -> IO (Probs String)
parseAndTrainClass docStr = do
  parsed <- parseString docStr
  return $ trainClass parsed

