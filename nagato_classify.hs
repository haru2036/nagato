{-# LANGUAGE DeriveDataTypeable #-}
import System.IO
import Control.Monad
import Data.List
import Data.Tuple
import Data.Map
import System.IO.UTF8 as S
import qualified NagatoIO as NagatoIO 
import qualified MeCabTools as MeCabTools

classify :: [(String, Float)] -> String
calcProbability :: [String] -> Map String Float -> Float
loadFileToClassify :: String -> IO String
makeProbabilityList :: [String] -> [(String, Map String Float)] -> [(String, Float)]

lookupPropabilityOfWord :: String -> Map String Float -> Float

main = do
  classes <- NagatoIO.readFromFile "classes.bin"
  readedSentence <- loadFileToClassify "toclassify.txt" 
  S.putStrLn readedSentence
  parsedSentence <- MeCabTools.parseWakati readedSentence
  let propabilityList = makeProbabilityList (words parsedSentence) classes
  System.IO.print propabilityList
  System.IO.putStrLn $ classify propabilityList

calcProbability words classMap = product $ Data.List.map (\a -> lookupPropabilityOfWord a classMap) words

lookupPropabilityOfWord word classMap = maybe 1.0 (\a -> a + 1.0) (Data.Map.lookup word classMap)

loadFileToClassify fileName = do
  handle <- openFile fileName ReadMode
  contents <- S.hGetContents handle
  return $ contents

makeProbabilityList words classes = Data.List.map (\a -> (fst a, calcProbability words (snd a))) classes 

classify propabilityList =let valueList = snd (unzip propabilityList)
                          in maybe "error" (\a -> fst (propabilityList !! a)) $ (Data.List.elemIndex (maximum valueList)) valueList
