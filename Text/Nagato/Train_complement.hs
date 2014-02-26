module Text.Nagato.Train_complement(
  doTrain
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
import System.IO.UTF8 as S
import Text.Nagato.Models as Models
import qualified Text.Nagato.NagatoIO as NagatoIO
import qualified Text.Nagato.Train as Train

makeComplementClass :: String -> [(String, Freqs)] -> Freqs
getClassNameList :: Map String (Freqs) -> [String]
twoMapKeys :: Freqs -> Freqs -> [String]
addClasses :: [Freqs] -> Freqs
getOtherClasses :: String -> Map String (Freqs) -> Freqs
addTwoMaps :: Freqs -> Freqs -> Freqs
removeMaybe :: Maybe Int -> Int

removeMaybe mb = maybe 0 (\a -> a) mb

twoMapKeys one two = nub $ (keys one) ++ (keys two)

addTwoMaps one two = fromList $ Data.List.map (\key -> (key, (removeMaybe (Data.Map.lookup key one)) + (removeMaybe (Data.Map.lookup key two)))) $ twoMapKeys one two

getOtherClasses className classes = addClasses $ snd $ unzip $ toList $ Data.Map.delete className classes

getClassNameList classes = keys classes

addClasses maps = if (length maps) <= 1
  then head maps
  else addTwoMaps (head maps) $ addClasses $ tail maps

makeComplementClass className classes = getOtherClasses className $ fromList classes

main = do
  counted <- Train.countFromSetting "classes.json"
  let complementCounts = Data.List.map (\classItems -> ((fst classItems), (Train.calcParameterForClass (makeComplementClass (fst classItems) counted)2))) counted
  NagatoIO.writeToFile "complementClasses.bin" complementCounts

doTrain :: String -> IO()
doTrain fileName = do
  counted <- Train.countFromSetting fileName
  let complementCounts = Data.List.map (\classItems -> ((fst classItems), (Train.calcParameterForClass (makeComplementClass (fst classItems) counted)2))) counted
  NagatoIO.writeToFile "complementClasses.bin" complementCounts

