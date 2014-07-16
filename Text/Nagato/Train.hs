{-# LANGUAGE DeriveDataTypeable #-}
module Text.Nagato.Train
( countFromSetting
  ,calcParameterForClass
  ,doTrain
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

trainClass :: String -> IO Freqs
trainClass inputString = do
  parseResult <- MeCabTools.parseFilteredChasenFormat inputString ["名詞"]
  return $ getUnigramFrequency $ parseResult 

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

doTrain :: String -> String -> IO()
doTrain settingFile saveFileName = do
  trainResult <- trainFromSetting settingFile
  NagatoIO.writeToFile saveFileName trainResult

trainFromSetting :: String -> IO [(String, Props)]
trainFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> trainClass a) classStrings
  let classesTrained = L.map (\a -> calcParameterForClass a 2) classesCounted
  return $ zip (fst unzippedClasses) classesTrained

countFromSetting :: String -> IO [(String, Freqs)]
countFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> trainClass a) classStrings
  return $ zip (fst unzippedClasses) classesCounted

calcParameterForClass :: Freqs -> Int -> Props
calcParameterForClass classMap alpha = Data.Map.map (\a -> ((realToFrac (a + 1) / (realToFrac ((sum (elems classMap) + (length (keys classMap)) * (alpha - 1))))))) classMap
