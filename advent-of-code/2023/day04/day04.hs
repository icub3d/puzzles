import Data.Bits (shift)
import Data.List (foldl')
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.Parsec as Parsec

data Card = Card
  { winners :: Set Int,
    ours :: Set Int
  }
  deriving (Show)

parseCard :: Parsec.Parsec String () Card
parseCard = do
  Parsec.string "Card"
  Parsec.spaces
  Parsec.many1 Parsec.digit
  Parsec.char ':'
  Parsec.spaces
  winners <- parseInts
  Parsec.spaces
  Parsec.char '|'
  Parsec.spaces
  ours <- parseInts
  return $ Card (Set.fromList winners) (Set.fromList ours)

parseInts :: Parsec.Parsec String () [Int]
parseInts = do
  ints <- Parsec.many1 Parsec.digit `Parsec.endBy` Parsec.spaces
  return $ map read ints

parseCards :: Parsec.Parsec String () [Card]
parseCards = Parsec.many1 parseCard

parser :: String -> [Card]
parser input = case Parsec.parse parseCards "" input of
  Left e -> error $ show e
  Right r -> r

p1 :: [Card] -> Int
p1 cards = sum $ map (points . matches) cards
  where
    matches (Card w o) = length $ Set.intersection w o
    points x = if x == 0 then 0 else 1 `shift` (x - 1)

main :: IO ()
main = do
  input <- readFile "input"
  let cards = parser input
  putStrLn $ "p1: " ++ show (p1 cards)
