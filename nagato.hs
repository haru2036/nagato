{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import Text.MeCab
import Text.JSON
import Text.JSON.Generic
import Data.List
import Data.Tuple
import Data.HashMap.Lazy

searchAndCountWords :: String -> [String] -> Int
getUnigramFrequency :: [String] -> HashMap String Int
trainClass :: String -> IO (HashMap String Int)
loadSettings :: String -> IO [(String, String)]


data AClass = AClass {
  className :: String,
  dataSource :: String
  }deriving(Eq, Show, Data, Typeable)

data ClassList = ClassList{
  classes :: [AClass]
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
  return $ decodeJSON contents :: IO [(String, String)]


main = do
  line <- readLn
  return $ trainClass line
