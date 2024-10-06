{-# LANGUAGE DeriveGeneric #-}

import Data.HashSet (HashSet, empty, fromList, insert, member, union)
import Data.Hashable
import Data.List (tails)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace
import GHC.Generics (Generic)

newtype Point = Point (Int, Int) deriving (Show, Eq, Generic)

instance Hashable Point

newtype Path = Path [Point] deriving (Show)

newtype Obstructions = Obstructions (HashSet Point)

instance Show Obstructions where
  show (Obstructions o) = unlines [[if Point (x, y) `member` o then '#' else '.' | x <- [minx .. maxx]] | y <- [miny .. maxy]]
    where
      minx = foldr (\(Point (x, _)) v -> min v x) 10000 o
      miny = foldr (\(Point (_, y)) v -> min v y) 10000 o
      maxx = foldr (\(Point (x, _)) v -> max v x) 0 o
      maxy = foldr (\(Point (_, y)) v -> max v y) 0 o

parse :: String -> Path
parse = Path . map (readPoint . splitOn ",") . splitOn " -> "
  where
    readPoint [x, y] = Point (read x, read y)

add :: Path -> HashSet Point -> HashSet Point
add (Path points) s = foldr (\[p1, p2] s -> addLine p1 p2 s) s (windows 2 points)
  where
    -- Actually makes a rectangle, but for well-formed input either x1 == x2 or y1 == y2
    makeLine :: Point -> Point -> [Point]
    makeLine (Point (x1, y1)) (Point (x2, y2)) = [Point (x, y) | x <- interval x1 x2, y <- interval y1 y2]
      where
        interval a b = [min a b .. max a b]
    addLine :: Point -> Point -> HashSet Point -> HashSet Point
    addLine p1 p2 s = fromList (makeLine p1 p2) `union` s

    windows :: Int -> [a] -> [[a]]
    windows m = foldr (zipWith (:)) (repeat []) . take m . tails

fall :: Point -> HashSet Point -> Maybe [Point]
fall (Point (x, y)) s
  | Point (x, y) `member` s = Nothing -- falling inside something does not make sense
  | not $ down `member` s = Just $ continue down
  | not $ left `member` s = Just $ continue left
  | not $ right `member` s = Just $ continue right
  | otherwise = Just [Point (x, y)]
  where
    down = Point (x, y + 1)
    left = Point (x - 1, y + 1)
    right = Point (x + 1, y + 1)
    continue p = p : fromJust (fall p s)

fallFromWithAbyss :: Int -> Point -> HashSet Point -> Maybe Point
fallFromWithAbyss abyss start stones =
  if fallsIntoAbyss trajectory
    then Nothing
    else Just (last trajectory)
  where
    -- fromJust is Justified because in Part 1 we don't fill up
    trajectory = fromJust $ fall start stones
    -- a falling trajectory will either be finite or contain a Point in the abyss
    fallsIntoAbyss :: [Point] -> Bool
    fallsIntoAbyss = any (\(Point (_, y)) -> y > abyss)

dropSandWithAbyss :: Int -> Point -> HashSet Point -> [Point]
dropSandWithAbyss abyss start stones = case fallFromWithAbyss abyss start stones of
  Nothing -> []
  Just end -> end : dropSandWithAbyss abyss start (end `insert` stones)

fallFromWithFloor :: Int -> Point -> HashSet Point -> Maybe Point
fallFromWithFloor floor start stones = last . takeWhile (\(Point (_, y)) -> y /= floor) <$> fall start stones

dropSandWithFloor :: Int -> Point -> HashSet Point -> [Point]
dropSandWithFloor floor start stones =
  let end = fallFromWithFloor floor start stones
   in case end of
    Nothing -> []
    Just end' -> end' : dropSandWithFloor floor start (end' `insert` stones)

main = do
  contents <- readFile "input14.txt"
  let stones = foldr (add . parse) empty $ lines contents
  let abyss = foldr (\(Point (_, y)) v -> max v y) 0 stones
  print $ length $ dropSandWithAbyss abyss (Point (500, 0)) stones
  let floor = abyss + 2
  print $ length $ dropSandWithFloor floor (Point (500, 0)) stones
