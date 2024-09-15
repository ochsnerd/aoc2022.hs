import Data.Char (isDigit)
import Data.Function (on)
import Data.List (elemIndex, sortBy)

data Token = Number Int | Comma | Open | Close deriving (Show, Eq)

parse :: String -> [Token]
parse = reverse . snd . parse' . (,[])
  where
    parse' :: (String, [Token]) -> (String, [Token])
    parse' ("", xs) = ("", xs)
    parse' (c : cs, xs) = case c of
      '[' -> parse' (cs, Open : xs)
      ']' -> parse' (cs, Close : xs)
      ',' -> parse' (cs, Comma : xs)
      _ -> parse' (rest, Number int : xs)
      where
        (rest, int) = eagerlyInt c cs
        eagerlyInt i is = (dropWhile isDigit is, read (i : takeWhile isDigit is))

-- eval :: [Token] -> _   <-- what type here? (want to make haskell lists)

ordered :: [Token] -> [Token] -> Bool
ordered (Number l : ls) (Number r : rs) = case compare l r of
  LT -> True
  GT -> False
  EQ -> ordered ls rs
ordered (Open : ls) (Open : rs) = ordered ls rs
ordered (Comma : ls) (Comma : rs) = ordered ls rs
ordered (Close : ls) (Close : rs) = ordered ls rs
ordered (Close : ls) _ = True
ordered _ (Close : rs) = False
ordered (Number l : ls) rs = ordered ([Open, Number l, Close] ++ ls) rs
ordered ls (Number r : rs) = ordered ls ([Open, Number r, Close] ++ rs)

orderedParse :: String -> String -> Bool
orderedParse s1 s2 = ordered (parse s1) (parse s2) -- <-- this is the Psi-Combinator

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = take n l : chunksOf n (drop n l)

-- Too lazy for import
fromJust :: Maybe a -> a
fromJust (Just x) = x

-- Part 2
findAndMul :: [[Token]] -> [Token] -> [Token] -> Int
findAndMul ts l r = idx l ts * idx r ts
  where
    idx :: (Eq a) => a -> [a] -> Int
    idx e l = ((+) 1 . fromJust) (elemIndex e l)

ordering :: [Token] -> [Token] -> Ordering
ordering l r
  | l == r = EQ -- this will not influence answer, not sure if relevant in general
  | ordered l r = LT
  | otherwise = GT

-- on = Ψ
-- https://www.uiua.org/docs/combinators
orderedParse' :: String -> String -> Bool
orderedParse' = on ordered parse

cursedIdx' :: (Eq a) => a -> [a] -> Int
cursedIdx' = ((.) . (.)) ((+) 1 . fromJust) elemIndex -- https://drewboardman.github.io/jekyll/update/2020/01/14/blackbird-operator.html

findAndMul' :: [[Token]] -> [Token] -> [Token] -> Int
findAndMul' l = on (*) ((+) 1 . fromJust . flip elemIndex l) -- again Psi!

main = do
  contents <- readFile "input13.txt"
  -- part 1
  (print . sum . zipWith (\i (l : r : _) -> if orderedParse l r then i else 0) [1 ..] . chunksOf 3 . lines) contents
  -- part 2
  let two = [Open, Open, Number 2, Close, Close]
  let six = [Open, Open, Number 6, Close, Close]
  (print . (\l -> findAndMul l two six) . sortBy ordering . (\l -> two : six : l) . map parse . filter (/= "") . lines) contents
