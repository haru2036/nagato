{-# LANGUAGE DeriveDataTypeable #-}
module Nagato_train
( countFromSetting
  ,calcParameterForClass
)where
import System.IO
import Control.Monad
import Text.MeCab
import Text.JSON
import Text.JSON.Generic
import Data.List
import Data.Tuple
import Data.Map
import Data.Serialize
import Models
import System.IO.UTF8 as S
import NagatoIO as NagatoIO
import MeCabTools as MeCabTools

searchAndCountWords :: String -> [String] -> Int
getUnigramFrequency :: [String] -> Freqs
calcParameterForClass :: Freqs -> Int -> Props

countFromSetting :: String -> IO [(String, Freqs)]

trainFromSetting :: String -> IO [(String, Props)]
trainClass :: String -> IO Freqs
loadSettings :: String -> IO [(String, String)]
loadClassStrings :: [String] -> IO [String]
filenameToString :: String -> IO String

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


searchAndCountWords key items = length $ Data.List.filter (==key) items

getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

trainClass inputString = do
  parseResult <- MeCabTools.parseFilteredChasenFormat inputString ["名詞"]
  return $ getUnigramFrequency $ parseResult 

loadSettings settingName = do
  handle <- openFile settingName ReadMode
  contents <- S.hGetContents handle
  return $ Data.List.map(\a -> (className a, dataSource a))(classes(decodeJSON contents :: ClassList))

filenameToString fileName = do
  handle <- openFile fileName ReadMode
  contents <- S.hGetContents handle
  return $ contents

loadClassStrings fileNames = do
  if length fileNames == 1
    then do
      str <- filenameToString $ head fileNames
      return [str]
    else do
      str <- filenameToString $ head fileNames
      deepStrs <- loadClassStrings $ drop 1 fileNames
      return $ str : deepStrs

main = do
  trainResult <- trainFromSetting "classes.json"
  NagatoIO.writeToFile "classes.bin" trainResult

trainFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> trainClass a) classStrings
  let classesTrained = Data.List.map (\a -> calcParameterForClass a 2) classesCounted
  return $ zip (fst unzippedClasses) classesTrained

countFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> trainClass a) classStrings
  return $ zip (fst unzippedClasses) classesCounted

calcParameterForClass classMap alpha = Data.Map.map (\a -> ((realToFrac (a + 1) / (realToFrac ((sum (elems classMap) + (length (keys classMap)) * (alpha - 1))))))) classMap
