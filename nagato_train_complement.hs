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
import System.IO.UTF8 as S
import qualified NagatoIO as NagatoIO

normalClassesToComplementClasses :: [(String, Map String Float)] -> [(String Map String Float)]
getClassNameList :: Map String (Map String Float) -> [String]
addClasses :: [Map String Float] -> Map String Float
getOtherClasses :: String -> Map String (Mao String Float) -> Map String Float

getOtherClasses className classes = snd $ unzip $ toList $ delete className classes

getClassNameList classes = keys classes

main = do
  normalClasses <- NagatoIO.readFromFile "classes.bin"
