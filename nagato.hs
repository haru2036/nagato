import Text.MeCab
import Data.List
import Data.Tuple
import Data.HashMap.Lazy

searchAndCountWords :: String -> [String] -> Int
getUnigramFrequency :: [String] -> HashMap String Int
train :: String -> IO (HashMap String Int )

searchAndCountWords key items = length $ Data.List.filter (==key) items
getUnigramFrequency sList = fromList [(a, searchAndCountWords a sList) | a <- nub sList]

train inputString = do
  mecab <- new2 "-Owakati"
  parseResult <- parse mecab inputString
  return $ getUnigramFrequency $ words parseResult 

main = do
  line <- readLn
  train line
