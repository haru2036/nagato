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
import Models
import qualified NagatoIO as NagatoIO
import qualified Nagato_train as Nagato_train

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
  counted <- Nagato_train.countFromSetting "classes.json"
  let complementCounts = Data.List.map (\classItems -> ((fst classItems), (Nagato_train.calcParameterForClass (makeComplementClass (fst classItems) counted)2))) counted
  NagatoIO.writeToFile "complementClasses.bin" complementCounts
