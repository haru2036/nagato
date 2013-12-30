{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import Control.Monad
import Text.MeCab
import Text.JSON
import Text.JSON.Generic
import Data.List
import Data.Tuple
import Data.Map
import Data.Serialize
import qualified ClassRW as ClassRW

searchAndCountWords :: String -> [String] -> Int
getUnigramFrequency :: [String] -> Map String Int

trainFromSetting :: String -> IO [(String, Map String Int)]
trainClass :: String -> IO (Map String Int)
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
  frequency :: Map String Int
  }deriving(Eq, Show, Data, Typeable)

data FrequencyOfClasses = FrequencyOfClasses{
  classesLearned :: Map String Frequency
  }deriving(Eq, Show, Data, Typeable)


searchAndCountWords key items = length $ Data.List.filter (==key) items

getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

trainClass inputString = do
  mecab <- new2 "-Owakati"
  parseResult <- parse mecab inputString
  return $ getUnigramFrequency $ words parseResult 

loadSettings settingName = do
  handle <- openFile settingName ReadMode
  contents <- hGetContents handle
  return $ Data.List.map(\a -> (className a, dataSource a))(classes(decodeJSON contents :: ClassList))

filenameToString fileName = do
  handle <- openFile fileName ReadMode
  contents <- hGetContents handle
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
  ClassRW.writeToFile "classes.bin" trainResult

trainFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classTrained <- mapM (\a -> trainClass a)classStrings
  return $ zip (fst unzippedClasses) classTrained

