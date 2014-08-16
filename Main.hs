import Text.Nagato.Util

main :: IO()
main = do
  putStrLn "Train : 1"
  putStrLn "Classify : 2"
  putStrLn "Test : 3"
  putStrLn "Please input number:"
  menu <- getLine
  case menu of
    "1" -> doTrain
    "2" -> doClassify
    "3" -> testAccuracy 
    otherwise -> return ()

doTrain :: IO()
doTrain = do
  putStrLn "Please input setting file name:"
  fileName <- getLine
  doTrainNormal fileName
  doTrainComplemnt fileName

