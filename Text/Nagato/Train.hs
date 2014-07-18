{-# LANGUAGE DeriveDataTypeable #-}
module Text.Nagato.Train
( countFromSetting
  ,trainFromSetting
  ,trainAndSaveFromSetting 
  ,freqsToProps
  ,trainClass
  ,countClass
  ,parseAndCountClass 
  ,parseAndTrainClass 
)where
import Text.CSV
import Data.List as L
import Data.Map
import Text.Nagato.Models
import Text.Nagato.NagatoIO as NagatoIO
import Text.Nagato.MeCabTools as MeCabTools


searchAndCountWords :: String -> [String] -> Int
searchAndCountWords key items = length $ L.filter (==key) items

getUnigramFrequency :: [String] -> Freqs
getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

parseString :: String -> IO [String]
parseString doc = MeCabTools.parseFilteredChasenFormat doc ["名詞"]

countClass :: [String] -> Freqs
countClass = getUnigramFrequency 

trainClass :: [String] -> Props
trainClass doc = (freqsToProps . countClass) doc 2 

freqsToProps :: Freqs -> Int -> Props
freqsToProps classMap alpha = Data.Map.map (\a -> ((realToFrac (a + 1) / (realToFrac ((sum (elems classMap) + (length (keys classMap)) * (alpha - 1))))))) classMap

parseAndCountClass :: String -> IO Freqs
parseAndCountClass docStr = do
  parsed <- parseString docStr
  return $ countClass parsed

parseAndTrainClass :: String -> IO Props
parseAndTrainClass docStr = do
  parsed <- parseString docStr
  return $ trainClass parsed


loadSettings :: String -> IO [(String, String)]
loadSettings settingName = do
  eitherCsv <- parseCSVFromFile settingName
  case eitherCsv of
    Right csv' -> return $ L.map (\x -> (x !! 0, x !! 1)) $ init csv'
    Left e -> error $ show e

loadClassStrings :: [String] -> IO [String]
loadClassStrings settingFiles = do
  if length settingFiles == 1
    then do
      str <- NagatoIO.loadPlainText $ head settingFiles
      return [str]
    else do
      str <- NagatoIO.loadPlainText $ head settingFiles
      deepStrs <- loadClassStrings $ drop 1 settingFiles
      return $ str : deepStrs

trainAndSaveFromSetting :: String -> String -> IO()
trainAndSaveFromSetting settingFile saveFileName = do
  trainResult <- trainFromSetting settingFile
  NagatoIO.writeToFile saveFileName trainResult

trainFromSetting :: String -> IO [(String, Props)]
trainFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesTrained <- mapM (\a -> parseAndTrainClass a) classStrings
  return $ zip (fst unzippedClasses) classesTrained

countFromSetting :: String -> IO [(String, Freqs)]
countFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> parseAndCountClass a) classStrings
  return $ zip (fst unzippedClasses) classesCounted

