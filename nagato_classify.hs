import System.IO
import Control.Monad
import Data.List
import Data.Tuple
import Data.Map
import qualified NagatoIO as NagatoIO 
import qualified MeCabTools as MeCabTools
import System.IO.UTF8 as S
import Models 


classify :: [(String, Float)] -> String
classifyByComplementClasses :: [(String, Float)] -> String
calcProbability :: [String] -> Props -> Float
makeProbabilityList :: [String] -> [(String, Props)] -> [(String, Float)]
lookupPropabilityOfWord :: String -> Props -> Float
classifyIO :: IO()
classifyComplementIO :: IO()

main = do
  classifyIO
  classifyComplementIO

classifyIO = do
  classes <- NagatoIO.readFromFile "classes.bin"
  readedSentence <- NagatoIO.loadFileToClassify "toclassify.txt" 
  parsedSentence <- MeCabTools.parseWakati readedSentence
  let propabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print propabilityList
  System.IO.putStrLn $ classify propabilityList

classifyComplementIO = do
  classes <- NagatoIO.readFromFile "complementClasses.bin"
  readedSentence <- NagatoIO.loadFileToClassify "toclassify.txt" 
  parsedSentence <- MeCabTools.parseWakati readedSentence
  let propabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print propabilityList
  System.IO.putStrLn $ classifyByComplementClasses propabilityList

calcProbability words classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) words

lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

makeProbabilityList words classes = Data.List.map (\a -> (fst a, calcProbability words (snd a))) classes 

classifyByComplementClasses propabilityList = let valueList = snd (unzip propabilityList)
                          in maybe "error" (\a -> fst (propabilityList !! a)) $ (Data.List.elemIndex (minimum valueList)) valueList

classify propabilityList = let valueList = snd (unzip propabilityList)
                          in maybe "error" (\a -> fst (propabilityList !! a)) $ (Data.List.elemIndex (maximum valueList)) valueList
