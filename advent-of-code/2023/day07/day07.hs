import Data.List (sort)
import Data.Map (Map, elems, empty, foldrWithKey, insertWith)
import Text.Parsec
  ( Parsec,
    char,
    digit,
    endBy,
    many1,
    oneOf,
    parse,
    spaces,
  )

data Hand = Hand {cards :: [Int], bid :: Int, jokers :: Bool} deriving (Show, Eq)

parseCard :: Bool -> Char -> Int
parseCard _ 'A' = 14
parseCard _ 'K' = 13
parseCard _ 'Q' = 12
parseCard False 'J' = 11
parseCard True 'J' = 1
parseCard _ 'T' = 10
parseCard _ c = read [c]

parseHand :: Bool -> Parsec String () Hand
parseHand jokers = do
  cards <- many1 (oneOf "AKQJT98765432")
  spaces
  bid <- many1 digit
  return $ Hand (map (parseCard jokers) cards) (read bid) jokers

parseHands :: Bool -> Parsec String () [Hand]
parseHands jokers = do
  endBy (parseHand jokers) (char '\n')

handCounts :: Hand -> Map Int Int
handCounts (Hand cards _ False) = foldl (\m c -> insertWith (+) c 1 m) empty cards
handCounts (Hand cards _ True) = both
  where
    jokers = length $ filter (== 1) cards
    counts = foldl (\m c -> insertWith (+) c 1 m) empty $ filter (/= 1) cards
    largest = fst $ foldrWithKey (\k v (k', v') -> if v > v' then (k, v) else (k', v')) (0, 0) counts
    both = insertWith (+) largest jokers counts

handValue :: Map Int Int -> Int
handValue counts = case length counts of
  1 -> 7
  2 -> case maximum $ elems counts of
    4 -> 6
    _ -> 5
  3 -> case maximum $ elems counts of
    3 -> 4
    _ -> 3
  4 -> 2
  _ -> 1

instance Ord Hand where
  compare (Hand c1 _ _) (Hand c2 _ j) =
    case compare (handValue $ handCounts (Hand c1 0 j)) (handValue $ handCounts (Hand c2 0 j)) of
      EQ -> compare c1 c2
      x -> x

main :: IO ()
main = do
  input <- readFile "input"
  let hands = sort $ case parse (parseHands False) "" input of
        Left _ -> []
        Right h -> h
  putStrLn $ "p1: " ++ show (sum $ zipWith (*) [1 ..] $ map (\(Hand _ bid _) -> bid) hands)
  let hands = sort $ case parse (parseHands True) "" input of
        Left _ -> []
        Right h -> h
  putStrLn $ "p2: " ++ show (sum $ zipWith (*) [1 ..] $ map (\(Hand _ bid _) -> bid) hands)
