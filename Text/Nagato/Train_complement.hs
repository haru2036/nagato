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


removeMaybe :: Maybe Int -> Int
removeMaybe mb = maybe 0 (\a -> a) mb

twoMapKeys :: Freqs -> Freqs -> [String]
twoMapKeys one two = nub $ (keys one) ++ (keys two)

addTwoMaps :: Freqs -> Freqs -> Freqs
addTwoMaps one two = fromList $ Data.List.map (\key -> (key, (removeMaybe (Data.Map.lookup key one)) + (removeMaybe (Data.Map.lookup key two)))) $ twoMapKeys one two

getOtherClasses :: String -> Map String (Freqs) -> Freqs
getOtherClasses className classes = addClasses $ snd $ unzip $ toList $ Data.Map.delete className classes

getClassNameList :: Map String (Freqs) -> [String]
getClassNameList classes = keys classes

addClasses :: [Freqs] -> Freqs
addClasses maps = if (length maps) <= 1
  then head maps
  else addTwoMaps (head maps) $ addClasses $ tail maps

makeComplementClass :: String -> [(String, Freqs)] -> Freqs
makeComplementClass className classes = getOtherClasses className $ fromList classes

doTrain :: String -> String -> IO()
doTrain settingFile saveFileName = do
  counted <- Train.countFromSetting settingFile
  let complementCounts = Data.List.map (\classItems -> ((fst classItems), (Train.calcParameterForClass (makeComplementClass (fst classItems) counted)2))) counted
  NagatoIO.writeToFile saveFileName complementCounts
