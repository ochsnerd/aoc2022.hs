import Data.Char (digitToInt)
import Data.List.Split (splitOn)

lineToSections :: String -> [[Int]]
lineToSections l = map (map read . splitOn "-")  (splitOn "," l)

contained :: [[Int]] -> Bool
contained [[a, b], [x, y]] | a <= x && b >= y = True
                           | x <= a && y >= b = True
                           | otherwise = False

overlap :: [[Int]] -> Bool
overlap [[a, b], [x, y]] | a <= y && b >= x = True
                         | y <= a && x >= b = True
                         | otherwise = False

main = do
  contents <- readFile "input4.txt"
  -- 1
  print $ foldr ((+) . fromEnum . contained . lineToSections) 0 (lines contents)
  -- 2
  print $ foldr ((+) . fromEnum . overlap . lineToSections) 0 (lines contents)
