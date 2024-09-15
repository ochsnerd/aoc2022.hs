{-# LANGUAGE ViewPatterns #-}

import Control.Lens ((??))
import Data.List (foldl', stripPrefix)
import Data.List.Split (chunksOf)

-- register is reversed (avoids repeatedly appending)
doCommand :: [Int] -> String -> [Int]
doCommand register "noop" = head register : register
doCommand register (stripPrefix "addx " -> Just offset) = (v + read offset) : v : register
  where
    v = head register

doCommands :: [String] -> [Int]
doCommands = reverse . foldl' doCommand [1]

signalStrengthAt :: Int -> [Int] -> Int
signalStrengthAt i l = (l !! (i - 1)) * i

renderPixel :: Int -> Int -> Char
renderPixel crtIndex registerValue =
  if abs (registerValue - crtIndex) < 2
    then '#'
    else '.'

render :: [Int] -> [String]
render = chunksOf 40 . zipWith renderPixel (cycle [0..39])

main = do
  contents <- readFile "input10.txt"
  let registerHistory = doCommands $ lines contents
  print $ sum $ signalStrengthAt <$> [20, 60 .. 220] ?? registerHistory
  mapM putStrLn $ render registerHistory
