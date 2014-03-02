import Text.CSV
import Data.List
import Text.Nagato.NagatoIO as NagatoIO
import Text.Nagato.Models as Models
import Text.Nagato.MeCabTools as MeCabTools
import qualified Text.Nagato.Classify as NC
import qualified Text.Nagato.Train as Train
import qualified Text.Nagato.Train_complement as Train_compl

main = do
  rawCsvData <- NagatoIO.loadCSVFileUtf8 "testData.csv"
  let csvData = init rawCsvData
  classes <- NagatoIO.readFromFile "classes.bin"
  classesComplement <- NagatoIO.readFromFile "complementClasses.bin"
  print $ fst $ unzip classes
  print $ fst $ unzip classesComplement
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
  return ((NC.classify wordList classNormal), (NC.classifyComplement wordList classComplement))

doTrainNormal :: String -> IO()
doTrainNormal settingName = Train.doTrain settingName "classes.bin"

doTrainComplemnt :: String -> IO()
doTrainComplemnt settingName = Train_compl.doTrain settingName "classes_complement.bin"
