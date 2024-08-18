import Data.List.Split (splitOn)
import Data.Maybe
import Control.Monad

data Hand = Rock | Paper | Scissors deriving (Enum, Eq, Show)

toGame1 :: (Char, Char) -> Maybe (Hand, Hand)
toGame1 (l, r) = liftA2 (,) (fromChar l) (fromChar r)
  where 
    fromChar 'A' = Just Rock
    fromChar 'B' = Just Paper
    fromChar 'C' = Just Scissors
    fromChar 'X' = Just Rock
    fromChar 'Y' = Just Paper
    fromChar 'Z' = Just Scissors
    fromChar _ = Nothing

toGame2 :: (Char, Char) -> Maybe (Hand, Hand)
toGame2 (l, r) = liftA2 (,) lHand (lHand >>= rHand r)
  where
    lHand :: Maybe Hand
    lHand = fromChar l
      where
        fromChar 'A' = Just Rock
        fromChar 'B' = Just Paper
        fromChar 'C' = Just Scissors
        fromChar _ = Nothing
    rHand :: Char -> Hand -> Maybe Hand
    rHand 'X' l = Just $ lose l
      where
        lose Rock = Scissors
        lose Scissors = Paper
        lose Paper = Rock
    rHand 'Y' l = Just l 
    rHand 'Z' l = Just $ win l
      where
        win Rock = Paper
        win Paper = Scissors
        win Scissors = Rock
    rHand _ _ = Nothing


fromLineWith :: ((Char, Char) -> Maybe (Hand, Hand)) -> String -> Maybe (Hand, Hand)
fromLineWith f s = (toChars >=> f) s
  where
    toChars :: String -> Maybe (Char, Char)
    toChars s = case words s of
                  [] -> Nothing
                  [_] -> Nothing
                  [l, r] -> if length l == 1 && length r == 1
                            then Just (head l, head r)
                            else Nothing
                  _ -> Nothing


score :: (Hand, Hand) -> Int
score (l, r) = shape r + outcome l r
  where
    shape Rock = 1
    shape Paper = 2
    shape Scissors = 3
    outcome l r | l == r      = 3
                | r `beats` l = 6
                | otherwise   = 0
                where
                  beats Rock Scissors = True
                  beats Paper Rock = True
                  beats Scissors Paper = True
                  beats _ _ = False


main = do
  contents <- readFile "input2.txt"
  -- 1
  -- lsp transformations
  print $ fmap sum $ sequence $ map (\x -> score <$> fromLineWith toGame1 x) $ lines contents
  print $ fmap sum $ mapM (\x -> score <$> fromLineWith toGame1 x) (lines contents)
  print $ sum <$> mapM (\x -> score <$> fromLineWith toGame1 x) (lines contents)
  print $ sum <$> mapM (fmap score . fromLineWith toGame1) (lines contents)
  -- 2
  print $ sum <$> mapM (fmap score . fromLineWith toGame2) (lines contents)
