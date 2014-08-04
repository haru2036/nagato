module Text.Nagato.Classify(
  calcProbability
  ,makeProbabilityList
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
  let ProbabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print ProbabilityList
  System.IO.putStrLn $ selectMost ProbabilityList

classifyComplementIO :: String -> IO()
classifyComplementIO classifySentence = do
  classes <- NagatoIO.readFromFile "complementClasses.bin"
  parsedSentence <- MeCabTools.parseWakati classifySentence
  let ProbabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print ProbabilityList
  System.IO.putStrLn $ selectMostByComplementClasses ProbabilityList

calcProbability :: [String] -> Probs -> Float
calcProbability wordList classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) wordList 

lookupPropabilityOfWord :: String -> Probs -> Float
lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

makeProbabilityList :: [String] -> [(String, Probs)] -> [(String, Float)]
makeProbabilityList wordList classes = Data.List.map (\a -> (fst a, calcProbability wordList (snd a))) classes 

selectMostByComplementClasses :: [(String, Float)] -> String
selectMostByComplementClasses ProbabilityList = let valueList = snd (unzip ProbabilityList)
                          in maybe "error" (\a -> fst (ProbabilityList !! a)) $ (Data.List.elemIndex (minimum valueList)) valueList

selectMost :: [(String, Float)] -> String
selectMost ProbabilityList = let valueList = snd (unzip ProbabilityList)
                          in maybe "error" (\a -> fst (ProbabilityList !! a)) $ (Data.List.elemIndex (maximum valueList)) valueList

classify :: [String] -> [(String, Probs)] -> String
classify wordList props = selectMost $ makeProbabilityList wordList props

classifyComplement :: [String] -> [(String, Probs)] -> String
classifyComplement wordList props = selectMostByComplementClasses $ makeProbabilityList wordList props
