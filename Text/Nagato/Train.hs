{-# LANGUAGE DeriveDataTypeable #-}
module Text.Nagato.Train
( countFromSetting
  ,calcParameterForClass
  ,doTrain
)where
import System.IO
import System.IO.UTF8 as S
import Text.JSON.Generic
import Data.List
import Data.Map
import Text.Nagato.Models
import Text.Nagato.NagatoIO as NagatoIO
import Text.Nagato.MeCabTools as MeCabTools

data AClass = AClass {
  className :: String,
  dataSource :: String
  }deriving(Eq, Show, Data, Typeable)

data ClassList = ClassList{
  classes :: [AClass]
  }deriving(Eq, Show, Data, Typeable)

data Frequency = Frequency{
  frequency :: Freqs
  }deriving(Eq, Show, Data, Typeable)

data FrequencyOfClasses = FrequencyOfClasses{
  classesLearned :: Map String Frequency
  }deriving(Eq, Show, Data, Typeable)


searchAndCountWords :: String -> [String] -> Int
searchAndCountWords key items = length $ Data.List.filter (==key) items

getUnigramFrequency :: [String] -> Freqs
getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

trainClass :: String -> IO Freqs
trainClass inputString = do
  parseResult <- MeCabTools.parseFilteredChasenFormat inputString ["名詞"]
  return $ getUnigramFrequency $ parseResult 

loadSettings :: String -> IO [(String, String)]
loadSettings settingName = do
  handle <- openFile settingName ReadMode
  contents <- S.hGetContents handle
  return $ Data.List.map(\a -> (className a, dataSource a))(classes(decodeJSON contents :: ClassList))

filenameToString :: String -> IO String
filenameToString settingFile = do
  handle <- openFile settingFile ReadMode
  contents <- S.hGetContents handle
  return $ contents

loadClassStrings :: [String] -> IO [String]
loadClassStrings settingFiles = do
  if length settingFiles == 1
    then do
      str <- filenameToString $ head settingFiles
      return [str]
    else do
      str <- filenameToString $ head settingFiles
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
  let classesTrained = Data.List.map (\a -> calcParameterForClass a 2) classesCounted
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
