module Text.Nagato.Nagato_classify(
  calcProbability
  ,makeProbabilityList
  ,classify
  ,classifyByComplementClasses
)where
import System.IO
import Data.List
import Data.Map
import Text.Nagato.NagatoIO as NagatoIO 
import Text.Nagato.MeCabTools as MeCabTools
import Text.Nagato.Models as Models


main = do
  readedSentence <- NagatoIO.loadPlainText "toclassify.txt" 
  classifyIO readedSentence
  classifyComplementIO readedSentence

classifyIO :: String -> IO()
classifyIO classifySentence = do
  classes <- NagatoIO.readFromFile "classes.bin"
  parsedSentence <- MeCabTools.parseWakati classifySentence
  let propabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print propabilityList
  System.IO.putStrLn $ classify propabilityList

classifyComplementIO :: String -> IO()
classifyComplementIO classifySentence = do
  classes <- NagatoIO.readFromFile "complementClasses.bin"
  parsedSentence <- MeCabTools.parseWakati classifySentence
  let propabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print propabilityList
  System.IO.putStrLn $ classifyByComplementClasses propabilityList

calcProbability :: [String] -> Props -> Float
calcProbability words classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) words

lookupPropabilityOfWord :: String -> Props -> Float
lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

makeProbabilityList :: [String] -> [(String, Props)] -> [(String, Float)]
makeProbabilityList words classes = Data.List.map (\a -> (fst a, calcProbability words (snd a))) classes 

classifyByComplementClasses :: [(String, Float)] -> String
classifyByComplementClasses propabilityList = let valueList = snd (unzip propabilityList)
                          in maybe "error" (\a -> fst (propabilityList !! a)) $ (Data.List.elemIndex (minimum valueList)) valueList

classify :: [(String, Float)] -> String
classify propabilityList = let valueList = snd (unzip propabilityList)
                          in maybe "error" (\a -> fst (propabilityList !! a)) $ (Data.List.elemIndex (maximum valueList)) valueList

