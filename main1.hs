import Data.List.Split (splitOn)
import Data.List (sortOn)

readInt :: String -> Int
readInt = read

main = do
  contents <- readFile "input1.txt"
  -- Solution to part 1
  print $ foldr (max . sum . map readInt . words) 0 (splitOn "\n\n" contents)
  -- Solution to part 2
  print $ sum $ take 3 $ sortOn negate $ map (sum . map readInt . words) (splitOn "\n\n" contents)
