{-# LANGUAGE ViewPatterns #-}

import Data.Foldable
import Data.List (isPrefixOf, stripPrefix, sort)
import Data.Monoid
import Debug.Trace

type Name = String

type Size = Int

data FSItem = File Name Size | Directory Name [FSItem] deriving (Show)

fsSize :: FSItem -> Size
fsSize (File _ size) = size
fsSize (Directory _ contents) = getSum $ foldMap size' contents
  where
    size' (File _ size) = Sum size
    size' (Directory _ contents) = foldMap size' contents

dirSizes :: FSItem -> [Size]
dirSizes (File _ _) = []
dirSizes (Directory _ contents) = size : foldMap dirSizes contents
  where
    size = foldr (\i s -> s + fsSize i) 0 contents

-- using Zipper as described in https://learnyouahaskell.com/zippers#a-very-simple-file-system
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)

type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Directory name (ls ++ [item] ++ rs), bs)

fsToRoot :: FSZipper -> FSZipper
fsToRoot zipper@(_, []) = zipper
fsToRoot zipper = fsToRoot $ fsUp zipper

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Directory dirName items, bs) = (item, FSCrumb dirName ls rs : bs)
  where
    nameIs name (Directory dirName _) = name == dirName
    nameIs name (File fileName _) = name == fileName
    (ls, item : rs) = break (nameIs name) items

fsAdd :: FSItem -> FSZipper -> FSZipper
fsAdd item (Directory name items, bs) = (Directory name (item : items), bs)

addFile :: Name -> Size -> FSZipper -> FSZipper
addFile name size = fsAdd (File name size)

addDir :: Name -> FSZipper -> FSZipper
addDir name = fsAdd (Directory name [])

addItem :: String -> FSZipper -> FSZipper
addItem (stripPrefix "dir " -> Just dirName) = addDir dirName
addItem s = addFile fileName (read size)
  where
    [size, fileName] = words s

doCommand :: String -> FSZipper -> FSZipper
doCommand "cd /" = fsToRoot
doCommand "cd .." = fsUp
doCommand (stripPrefix "cd " -> Just dirName) = fsTo dirName
doCommand "ls" = id -- ls does not change location in fs

processInputLine :: String -> FSZipper -> FSZipper
processInputLine (stripPrefix "$ " -> Just command) = doCommand command
processInputLine s = addItem s

main = do
  contents <- readFile "input7.txt"
  let (fs, _) = fsToRoot $ foldl' (flip processInputLine) (Directory "/" [], []) $ lines contents
  -- Part 1
  print $ sum $ filter (< 100001) $ dirSizes fs
  -- Part 2
  let spaceToFree = fsSize fs - (70000000 - 30000000)
  print $ minimum $ filter (> spaceToFree) $ dirSizes fs
  -- turns out I did not need to keep the names around
