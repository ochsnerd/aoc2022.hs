import Data.List
import Data.List.Split
import Data.Char (isSpace)

newtype Stacks = S [[Char]] deriving (Show)
newtype Instruction = I (Int, Int, Int) deriving (Show)

instance Semigroup Stacks where
  (<>) (S l) (S r) = S $ zipWith (++) l r

instance Monoid Stacks where
  mempty = S $ repeat []
  
isStackLine :: String -> Bool
isStackLine = isPrefixOf "[" . dropWhile isSpace

parseStackLine :: String -> Stacks
parseStackLine l = S $ map parseChunk (chunksOf 4 l)
  where
    parseChunk "   " = []
    parseChunk "    " = []
    parseChunk ['[', x, ']'] = [x]
    parseChunk ['[', x, ']', ' '] = [x]

heads :: Stacks -> [Char]
heads (S s) = map head s

isInstruction :: String -> Bool
isInstruction = isPrefixOf "move"

parseInstruction :: String -> Instruction
parseInstruction s = I (read amount, read from - 1, read to - 1)
  where
    ws = words s
    amount = ws !! 1
    from = ws !! 3
    to = ws !! 5

replaceAt :: Int -> a -> [a] -> [a]
replaceAt index newVal list = start ++ newVal : tail end
  where
    (start, end) = splitAt index list
    
applyInstruction :: Stacks -> Instruction -> Stacks
applyInstruction (S stacks) (I (amount, from, to)) = S $ replaceAt to newTo (replaceAt from newFrom stacks)
  where
    fromStack = stacks !! from
    toStack = stacks !! to
    newFrom = drop amount fromStack
    -- Task 1:
    -- newTo = reverse (take amount fromStack) ++ toStack
    -- Task 2:
    newTo = take amount fromStack ++ toStack

main = do
  contents <- readFile "input5.txt"
  let stacks = foldMap parseStackLine $ takeWhile isStackLine $ lines contents
  let instructions = map parseInstruction $ dropWhile (not . isInstruction) $ lines contents
  print $ heads $ foldl' applyInstruction stacks instructions
