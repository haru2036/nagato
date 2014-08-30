module Text.Nagato.Train_complement(
  makeComplementClass
)where
import qualified Data.List as List
import Data.Map as Map
import Text.Nagato.Models 
import Text.Nagato.Train

removeMaybe :: Maybe Int -> Int
removeMaybe mb = maybe 0 (\a -> a) mb

twoMapKeys :: Freqs String -> Freqs String -> [String]
twoMapKeys one two = List.nub $ (keys one) ++ (keys two)

addTwoMaps :: Freqs String -> Freqs String -> Freqs String
addTwoMaps one two = fromList $ List.map (\key -> (key, (removeMaybe (Map.lookup key one)) + (removeMaybe (Map.lookup key two)))) $ twoMapKeys one two

getOtherClasses :: String -> Map String (Freqs String) -> Freqs String
getOtherClasses className classes = addClasses $ snd $ unzip $ toList $ Map.delete className classes

getClassNameList :: Map String (Freqs String) -> [String]
getClassNameList classes = keys classes

addClasses :: [Freqs String] -> Freqs String
addClasses maps = List.foldl (\acc x -> addTwoMaps acc x) (head maps) (tail maps)

makeComplementCount :: String -> [(String, Freqs String)] -> Freqs String
makeComplementCount className classes = getOtherClasses className $ fromList classes

makeComplementClass :: String -> [(String, Freqs String)] -> Probs String
makeComplementClass name freqs = freqsToProbs (makeComplementCount name freqs) 2
