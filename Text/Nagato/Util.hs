module Text.Nagato.Util(
  doTrainNormal
 ,doTrainComplemnt
 ,doClassify
 ,testAccuracy 
)where
import Text.CSV
import Data.List as L
import Data.Maybe
import Text.Nagato.NagatoIO as NagatoIO
import Text.Nagato.Models as Models
import Text.Nagato.MeCabTools as MeCabTools
import qualified Text.Nagato.Classify as NC
import qualified Text.Nagato.Train as Train
import qualified Text.Nagato.Train_complement as Train_compl

testAccuracy :: IO()
testAccuracy = do
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

test :: String -> [(String, Probs String)] -> [(String, Probs String)] -> IO(String, String)
test text classNormal classComplement = do
  wakati <- MeCabTools.parseWakati text
  let wordList = words wakati
  return ((fromJust (NC.classify wordList classNormal)), (fromJust (NC.classifyComplement wordList classComplement)))

trainAndSaveFromSetting :: String -> String -> IO()
trainAndSaveFromSetting settingFile saveFileName = do
  trainResult <- trainFromSetting settingFile
  NagatoIO.writeToFile saveFileName trainResult

trainFromSetting :: String -> IO [(String, Probs String)]
trainFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesTrained <- mapM (\a -> Train.parseAndTrainClass a) classStrings
  return $ zip (fst unzippedClasses) classesTrained

doTrainNormal :: String -> IO()
doTrainNormal settingName = trainAndSaveFromSetting settingName "classes.bin"

doTrainComplemnt :: String -> IO()
doTrainComplemnt settingName = doTrainCompl settingName "classes_complement.bin"

loadClassStrings :: [String] -> IO [String]
loadClassStrings settingFiles = do
  if length settingFiles == 1
    then do
      str <- NagatoIO.loadPlainText $ head settingFiles
      return [str]
    else do
      str <- NagatoIO.loadPlainText $ head settingFiles
      deepStrs <- loadClassStrings $ drop 1 settingFiles
      return $ str : deepStrs

doTrainCompl :: String -> String -> IO()
doTrainCompl settingFile saveFileName = do
  counted <- countFromSetting settingFile
  let complementCounts = L.map (\classItems -> ((fst classItems), (Train_compl.makeComplementClass (fst classItems) counted))) counted
  NagatoIO.writeToFile saveFileName complementCounts

countFromSetting :: String -> IO [(String, Freqs String)]
countFromSetting settingFileName = do
  classesList <- loadSettings settingFileName
  let unzippedClasses = unzip classesList
  classStrings <- loadClassStrings $ snd unzippedClasses
  classesCounted <- mapM (\a -> Train.parseAndCountClass a) classStrings
  return $ zip (fst unzippedClasses) classesCounted

loadSettings :: String -> IO [(String, String)]
loadSettings settingName = do
  eitherCsv <- parseCSVFromFile settingName
  case eitherCsv of
    Right csv' -> return $ L.map (\x -> (x !! 0, x !! 1)) $ init csv'
    Left e -> error $ show e

doClassify :: IO()
doClassify = do
  classes <- NagatoIO.readFromFile "classes.bin"
  classesComplement <- NagatoIO.readFromFile "classes_complement.bin"
  print $ fst $ unzip classes
  print $ fst $ unzip classesComplement
  putStrLn "Please type in a sentence:"
  sentence <- getLine
  wordsWakati <- MeCabTools.parseWakati sentence
  let wordsList = words wordsWakati
  putStrLn "normal:"
  let classified = NC.classify wordsList classes
  print classified
  putStrLn "complement:"
  let classifiedComplement = NC.classifyComplement wordsList classesComplement
  print classifiedComplement
