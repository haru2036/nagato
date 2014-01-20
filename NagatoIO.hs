module Nagato.NagatoIO
( writeToFile
, readFromFile
, writeToFileCounts
, readFromFileCounts
)where

import System.IO
import Control.Monad
import Data.List
import Data.Tuple
import Data.Map
import Data.Serialize
import Data.ByteString
import Data.Either.Unwrap


writeToFile :: String -> [(String, Map String Float)] -> IO()
readFromFile :: String -> IO [(String, Map String Float)]
writeToFileCounts :: String -> [(String, Map String Int)] -> IO()
readFromFileCounts :: String -> IO [(String, Map String Int)]


writeToFile filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readFromFile filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String [(String, Map String Float)]
  return $ fromRight decoded

writeToFileCounts filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readFromFileCounts filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String [(String, Map String Int)]
  return $ fromRight decoded
