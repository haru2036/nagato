module NagatoIO
( writeToFile
, readFromFile
, writeToFileCounts
, readFromFileCounts
, loadFileToClassify
, loadCSVFileUtf8
)where

import System.IO
import Data.Map
import Data.Serialize
import Data.ByteString
import Data.Either.Unwrap
import Text.CSV
import System.IO.UTF8 as S

loadFileToClassify :: String -> IO String
loadFileToClassify fileName = do
  handle <- openFile fileName ReadMode
  contents <- S.hGetContents handle
  return $ contents

writeToFile :: String -> [(String, Map String Float)] -> IO()
writeToFile filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readFromFile :: String -> IO [(String, Map String Float)]
readFromFile filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String [(String, Map String Float)]
  return $ fromRight decoded

writeToFileCounts :: String -> [(String, Map String Int)] -> IO()
writeToFileCounts filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readFromFileCounts :: String -> IO [(String, Map String Int)]
readFromFileCounts filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String [(String, Map String Int)]
  return $ fromRight decoded

loadCSVFileUtf8 :: String -> IO CSV
loadCSVFileUtf8 fileName = do
  loadedText <- loadFileToClassify fileName
  let parsed = either (\a->[]) (\a->a) $ parseCSV "hoge" loadedText
  return parsed
