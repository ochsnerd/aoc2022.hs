import Data.List (intersect)
import Data.List.Split (chunksOf)
import Data.Char (ord, isAsciiUpper, isAsciiLower)

priority :: Char -> Int
priority c | isAsciiUpper c = ord c - 38
           | isAsciiLower c = ord c - 96
           | otherwise = undefined

itemsInBothCompartments :: String -> String
itemsInBothCompartments l = uncurry intersect (splitAt (length l `div` 2) l)

commonItems :: [String] -> String
commonItems [] = ""
commonItems l = foldr intersect ['A'..'z'] l

main = do
  contents <- readFile "input3.txt"
  -- 1
  print $ sum $ map (priority . head . itemsInBothCompartments) $ lines contents
  -- 2
  print $ sum $ map (priority . head . commonItems) $ (chunksOf 3 . lines) contents

  
  
  
