import Data.List

windows :: Int -> [a] -> [[a]]
windows m = foldr (zipWith (:)) (repeat []) . take m . tails

allUnique :: Eq a => [a] -> Bool
allUnique l = length l == length (nub l)

firstMarkerPosition :: Eq a => Int -> ([a] -> Bool) -> [a] -> Int
firstMarkerPosition markerLength markerCondition stream = length (takeWhile (not . allUnique) $ windows markerLength stream)

main = do
  contents <- readFile "input6.txt"
  -- Part 1
  print $ 4 + firstMarkerPosition 4 allUnique contents
  -- Part 2
  print $ 14 + firstMarkerPosition 14 allUnique contents
