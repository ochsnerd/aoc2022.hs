import Data.Char (ord)
import Data.Maybe (fromJust, mapMaybe)
import Data.Sequence (Seq, ViewL (..), viewl, (|>))
import Algorithm.Search (bfs)
import Data.Sequence qualified as Seq

-- rows x columns
data Grid = Grid Int Int [Char] deriving (Show)

-- row x col
newtype Position = Position (Int, Int) deriving (Show, Eq, Ord)

makeGrid :: String -> Grid
makeGrid s = Grid m n g
  where
    m = (length . lines) s
    n = (length . head . lines) s
    g = filter (/= '\n') s

at :: Grid -> Position -> Maybe Char
at (Grid n m e) (Position (i, j))
  | i < 0 || j < 0 = Nothing
  | i >= n || j >= m = Nothing
  | otherwise = Just (e !! idx)
  where
    idx = i * m + j

neighbors :: Position -> [Position]
neighbors (Position (i, j)) = map Position [(i - 1, j), (i, j + 1), (i + 1, j), (i, j - 1)]

moves :: Position -> Grid -> [Position]
moves p g = mapMaybe (\p' -> if islegal p' then Just p' else Nothing) $ neighbors p
  where
    here = ord . fromJust $ g `at` p
    islegal :: Position -> Bool
    -- Part 1 (upwards)
    islegal = maybe False (((here + 1) >=) . ord) . (g `at`)
    -- Part 2 (downwards)
    -- islegal = maybe False (((here - 1) <=) . ord) . (g `at`)

pathTo :: Grid -> Char -> Position -> Maybe [Position]
pathTo grid end = bfs (`moves` grid) found
  where
    found :: Position -> Bool
    found = (Just end ==) . (grid `at`)

allWithElevation :: Grid -> Char -> [Position]
allWithElevation g@(Grid m n es) e = filter (\p -> (g `at` p) == Just e) [Position (i, j) | i <- [0..m-1], j <- [0..n-1]]

main = do
  contents <- readFile "input12.txt"
  let grid = makeGrid contents
  -- Part 1
  -- print $ length . fromJust $ pathTo grid 'E' (Position (0,0))
  -- Part 2
  print $ fmap (1 +) $ minimum $
    map (fmap length . pathTo grid 'a') $
    filter ((Just 'z' ==) . (grid `at`)) (allWithElevation grid 'E' >>= neighbors)
