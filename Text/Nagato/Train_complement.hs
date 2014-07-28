module Text.Nagato.Train_complement(
  makeComplementClass
)where
import qualified Data.List as List
import Data.Map as Map
import Text.Nagato.Models 
import Text.Nagato.Train

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

makeComplementCount :: String -> [(String, Freqs)] -> Freqs
makeComplementCount className classes = getOtherClasses className $ fromList classes

makeComplementClass :: String -> [(String, Freqs)] -> Props
makeComplementClass name freqs = freqsToProps (makeComplementCount name freqs) 2
