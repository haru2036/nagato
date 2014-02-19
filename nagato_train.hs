{-# LANGUAGE DeriveDataTypeable #-}
module Nagato_train
( countFromSetting
  ,calcParameterForClass
)where
import System.IO
import Text.JSON.Generic
import Data.List
import Data.Map
import Models
import System.IO.UTF8 as S
import NagatoIO as NagatoIO
import MeCabTools as MeCabTools




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
filenameToString fileName = do
  handle <- openFile fileName ReadMode
  contents <- S.hGetContents handle
  return $ contents

loadClassStrings :: [String] -> IO [String]
loadClassStrings fileNames = do
  if length fileNames == 1
    then do
      str <- filenameToString $ head fileNames
      return [str]
    else do
      str <- filenameToString $ head fileNames
      deepStrs <- loadClassStrings $ drop 1 fileNames
      return $ str : deepStrs

main :: IO()
main = do
  trainResult <- trainFromSetting "classes.json"
  NagatoIO.writeToFile "classes.bin" trainResult

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
