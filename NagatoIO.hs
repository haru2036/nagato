module NagatoIO
( writeToFile
, readFromFile
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


writeToFile filePath classes = do
  let bytes = Data.Serialize.encode classes
  Data.ByteString.writeFile filePath bytes

readFromFile filePath = do
  bytes <- Data.ByteString.readFile filePath
  let decoded = Data.Serialize.decode bytes :: Either String [(String, Map String Float)]
  return $ fromRight decoded
