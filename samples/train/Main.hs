import Text.Nagato.Util
main :: IO()
main = do
  printLn "Please type input settings file name"
  fn <- getLine
  doTrain fn
  doTrainComplement fn
