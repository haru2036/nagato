module Text.Nagato.Train_complement(
  doTrain
)where
import qualified Data.List as List
import Data.Map as Map
import Text.Nagato.Models 
import qualified Text.Nagato.NagatoIO as NagatoIO
import qualified Text.Nagato.Train as Train


removeMaybe :: Maybe Int -> Int
removeMaybe mb = maybe 0 (\a -> a) mb

twoMapKeys :: Freqs -> Freqs -> [String]
twoMapKeys one two = List.nub $ (keys one) ++ (keys two)

addTwoMaps :: Freqs -> Freqs -> Freqs
addTwoMaps one two = fromList $ List.map (\key -> (key, (removeMaybe (Map.lookup key one)) + (removeMaybe (Map.lookup key two)))) $ twoMapKeys one two

getOtherClasses :: String -> Map String (Freqs) -> Freqs
getOtherClasses className classes = addClasses $ snd $ unzip $ toList $ Map.delete className classes

getClassNameList :: Map String (Freqs) -> [String]
getClassNameList classes = keys classes

addClasses :: [Freqs] -> Freqs
addClasses maps = List.foldl (\acc x -> addTwoMaps acc x) (head maps) (tail maps)

makeComplementClass :: String -> [(String, Freqs)] -> Freqs
makeComplementClass className classes = getOtherClasses className $ fromList classes

doTrain :: String -> String -> IO()
doTrain settingFile saveFileName = do
  counted <- Train.countFromSetting settingFile
  let complementCounts = List.map (\classItems -> ((fst classItems), (Train.calcParameterForClass (makeComplementClass (fst classItems) counted)2))) counted
  NagatoIO.writeToFile saveFileName complementCounts
