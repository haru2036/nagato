import Text.CSV
import Data.List
import Text.Nagato.NagatoIO as NagatoIO
import Text.Nagato.Models as Models
import Text.Nagato.MeCabTools as MeCabTools
import qualified Text.Nagato.Classify as NC
import qualified Text.Nagato.Train as Train
import qualified Text.Nagato.Train_complement as Train_compl

main = do
  classes <- NagatoIO.readFromFile "classes.bin"
  classesComplement <- NagatoIO.readFromFile "complementClasses.bin"
  print $ fst $ unzip classes
  print $ fst $ unzip classesComplement
  putStrLn "Please type in a sentence:"
  sentence <- readLn
  wordsWakati <- MeCabTools.parseWakati sentence
  let wordsList = words wordsWakati
  putStrLn "normal:"
  let classified = NC.classify wordsList classes
  print classified
  putStrLn "complement:"
  let classifiedComplement = NC.classifyComplement wordsList classesComplement
  print classifiedComplement
