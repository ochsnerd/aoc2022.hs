import Data.List (foldl')

type Worry = Int

type Index = Int

type MonkeyAction = Worry -> (Index, Worry)

type Items = [[Worry]]

type Counts = [Int]

append :: Items -> (Index, Worry) -> Items
append state (index, item) = take index state ++ [state !! index ++ [item]] ++ drop (index + 1) state

pop :: Items -> Index -> (Worry, Items)
pop state index = (item, take index state ++ [rest] ++ drop (index + 1) state)
  where
    item : rest = state !! index

increment :: Counts -> Index -> Counts
increment counts index = take index counts ++ [1 + counts !! index] ++ drop (index + 1) counts

monkey :: (Worry -> Worry) -> (Worry -> Bool) -> Index -> Index -> Worry -> (Index, Worry)
monkey operation test toTrue toFalse item =
  if test item'
    then (toTrue, item')
    else (toFalse, item')
  where
    -- part 1
    -- item' = operation item `div` 3
    -- part 2
    item' = operation item `mod` (13 * 19 * 11 * 17 * 3 * 7 * 5 * 2)

monkeyDoSingle :: MonkeyAction -> (Counts, Items) -> Index -> (Counts, Items)
monkeyDoSingle action (counts, state) index = (increment counts index, append state' (action item))
  where
    (item, state') = pop state index

monkeyDoAll :: MonkeyAction -> (Counts, Items) -> Index -> (Counts, Items)
monkeyDoAll action (counts, state) index = foldl' (monkeyDoSingle action) (counts, state) (replicate (length (state !! index)) index)

doRound :: [MonkeyAction] -> (Counts, Items) -> (Counts, Items)
doRound actions state = foldl' (\state (action, index) -> monkeyDoAll action state index) state (zip actions [0 ..])

divisible :: Int -> Int -> Bool
divisible a b = b `mod` a == 0

twoLargest :: [Int] -> Maybe (Int, Int)
twoLargest xs
  | length xs < 2 = Nothing
  | otherwise = Just (maximum xs, maximum (filter (/= maximum xs) xs))

main = do
  let (i0, m0) = ([53, 89, 62, 57, 74, 51, 83, 97], monkey (* 3) (divisible 13) 1 5)
  let (i1, m1) = ([85, 94, 97, 92, 56], monkey (+ 2) (divisible 19) 5 2)
  let (i2, m2) = ([86, 82, 82], monkey (+ 1) (divisible 11) 3 4)
  let (i3, m3) = ([94, 68], monkey (+ 5) (divisible 17) 7 6)
  let (i4, m4) = ([83, 62, 74, 58, 96, 68, 85], monkey (+ 4) (divisible 3) 3 6)
  let (i5, m5) = ([50, 68, 95, 82], monkey (+ 8) (divisible 7) 2 4)
  let (i6, m6) = ([75], monkey (* 7) (divisible 5) 7 0)
  let (i7, m7) = ([92, 52, 85, 89, 68, 82], monkey (\i -> i * i) (divisible 2) 0 1)
  let allMonkeyActions = [m0, m1, m2, m3, m4, m5, m6, m7]
  let startingState = [i0, i1, i2, i3, i4, i5, i6, i7]
  let (endCounts, endStates) = foldr (\_ state -> doRound allMonkeyActions state) ([0, 0, 0, 0, 0, 0, 0, 0], startingState) [1 .. 10000]
  print (endCounts, endStates)
  let Just (a, b) = twoLargest endCounts
  print (a * b)
