module Text.Nagato.Classify(
  calcprobability
  ,makeprobabilityList
  ,classify
  ,classifyComplement
)where
import System.IO
import Data.List
import Data.Map
import Text.Nagato.NagatoIO as NagatoIO 
import Text.Nagato.MeCabTools as MeCabTools
import Text.Nagato.Models as Models

main :: IO()
main = do
  readedSentence <- NagatoIO.loadPlainText "toclassify.txt" 
  classifyIO readedSentence
  classifyComplementIO readedSentence

classifyIO :: String -> IO()
classifyIO classifySentence = do
  classes <- NagatoIO.readFromFile "classes.bin"
  parsedSentence <- MeCabTools.parseWakati classifySentence
  let probabilityList = makeprobabilityList (words parsedSentence) classes
  System.IO.print probabilityList
  System.IO.putStrLn $ selectMost probabilityList

classifyComplementIO :: String -> IO()
classifyComplementIO classifySentence = do
  classes <- NagatoIO.readFromFile "complementClasses.bin"
  parsedSentence <- MeCabTools.parseWakati classifySentence
  let probabilityList = makeprobabilityList (words parsedSentence) classes
  System.IO.print probabilityList
  System.IO.putStrLn $ selectMostByComplementClasses probabilityList

calcprobability :: [String] -> Probs -> Float
calcprobability wordList classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) wordList 

lookupPropabilityOfWord :: String -> Probs -> Float
lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

makeprobabilityList :: [String] -> [(String, Probs)] -> [(String, Float)]
makeprobabilityList wordList classes = Data.List.map (\a -> (fst a, calcprobability wordList (snd a))) classes 

selectMostByComplementClasses :: [(String, Float)] -> String
selectMostByComplementClasses probabilityList = let valueList = snd (unzip probabilityList)
                          in maybe "error" (\a -> fst (probabilityList !! a)) $ (Data.List.elemIndex (minimum valueList)) valueList

selectMost :: [(String, Float)] -> String
selectMost probabilityList = let valueList = snd (unzip probabilityList)
                          in maybe "error" (\a -> fst (probabilityList !! a)) $ (Data.List.elemIndex (maximum valueList)) valueList

classify :: [String] -> [(String, Probs)] -> String
classify wordList props = selectMost $ makeprobabilityList wordList props

classifyComplement :: [String] -> [(String, Probs)] -> String
classifyComplement wordList props = selectMostByComplementClasses $ makeprobabilityList wordList props
