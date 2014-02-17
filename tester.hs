import Text.CSV
import Data.List
import NagatoIO
import qualified Nagato_classify as NC
import MeCabTools as MeCabTools
import Models


main = do
  rawCsvData <- NagatoIO.loadCSVFileUtf8 "testData.csv"
  let csvData = init rawCsvData
  classes <- NagatoIO.readFromFile "classes.bin"
  print $ fst $ unzip classes
  print $ fst $ unzip classesComplement
  classesComplement <- NagatoIO.readFromFile "complementClasses.bin"
  classed <- mapM (\x->test (head x) classes classesComplement) csvData
  let compared = unzip $ map (\a -> judge a) $ zip [a !! 1 | a <- csvData] classed
  let accuracyNormal = (realToFrac (length (filter (==True) (fst compared)))) / (realToFrac (length csvData))
  let accuracyComplement = (realToFrac (length (filter (==True) (snd compared)))) / (realToFrac (length csvData))
  putStrLn "normal:"
  print accuracyNormal
  putStrLn "complement:"
  print accuracyComplement

judge :: (String, (String, String)) -> (Bool, Bool)
judge item = 
  let trueAnswer = fst item
      answers = snd item
  in ((trueAnswer == (fst answers)),(trueAnswer == (snd answers )))

test :: String -> [(String, Props)] -> [(String, Props)] -> IO(String, String)
test text classNormal classComplement = do
  wakati <- MeCabTools.parseWakati text
  let wordList = words wakati
  return ((doClassify wordList classNormal), (doClassifyComplement wordList classComplement))


doClassify :: [String] -> [(String, Props)] -> String
doClassify wordList props = NC.classify $ NC.makeProbabilityList wordList props

doClassifyComplement :: [String] -> [(String, Props)] -> String
doClassifyComplement wordList props = NC.classifyByComplementClasses $ NC.makeProbabilityList wordList props
