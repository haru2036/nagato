{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import Control.Monad
import Data.List
import Data.Tuple
import Data.Map
import qualified NagatoIO as NagatoIO 


classify :: [String] -> [(String, Map String Float)] -> String
calcProbability :: [String] -> Map String Float -> Float

lookupPropabilityOfWord :: String -> Map String Float -> Float

main = do
  classes <- NagatoIO.readFromFile "classes.bin"
  readedSentence <- readLn
  parsedSentence <- NagatoIO.wakatiParse readedSentence
  putStrLn $ classify (words parsedSentence) classes

calcProbability words classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) words

lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

classify words classes = let propabilityList = Data.List.map (\a -> (fst a, calcProbability words (snd a))) classes 
                         in maybe "damedesita" (\a -> fst (propabilityList !! a)) $ Data.List.elemIndex (maximum (propabilityList)) propabilityList
