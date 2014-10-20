module Text.Nagato.Classify(
  calcprobability
  ,makeprobabilityList
  ,classify
  ,classifyComplement
)where
import System.IO()
import Data.List
import Data.Map
import Data.Maybe()
import Text.Nagato.NagatoIO as NagatoIO()
import Text.Nagato.MeCabTools as MeCabTools()
import Text.Nagato.Models as Models


calcprobability :: (Ord a) => [a] -> Probs a -> Float
calcprobability wordList classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) wordList 

lookupPropabilityOfWord :: (Ord a) => a -> Probs a -> Float
lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

makeprobabilityList :: (Ord a) => [a] -> [(b , Probs a)] -> [(b, Float)]
makeprobabilityList wordList classes = Data.List.map (\a -> (fst a, calcprobability wordList (snd a))) classes 

selectMostByComplementClasses :: [(a, Float)] -> Maybe a
selectMostByComplementClasses probabilityList = let valueList = snd (unzip probabilityList)
                          in do 
                            idx <- ((Data.List.elemIndex (minimum valueList)) valueList) 
                            let a = fst (probabilityList !! idx)
                            return a

selectMost :: [(a, Float)] -> Maybe a
selectMost probabilityList = let valueList = snd (unzip probabilityList)
                          in do 
                            idx <- ((Data.List.elemIndex (maximum valueList)) valueList) 
                            let a = fst (probabilityList !! idx)
                            return a

classify :: (Ord a, Ord b) => [a] -> [(b, Probs a)] -> Maybe b
classify wordList props = selectMost $ makeprobabilityList wordList props

classifyComplement :: (Ord a) => [a] -> [(b, Probs a)] -> Maybe b
classifyComplement wordList probs = selectMostByComplementClasses $ makeprobabilityList wordList probs
