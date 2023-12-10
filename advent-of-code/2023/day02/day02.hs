import Data.List.Split (splitOn)
import qualified Text.Parsec as Parsec

data Round = Round {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {rounds :: [Round], id :: Int} deriving (Show)

parseColor :: Parsec.Parsec String () (Int, String)
parseColor = do
  count <- Parsec.many1 Parsec.digit
  Parsec.spaces
  color <- Parsec.many1 Parsec.letter
  return (read count :: Int, color)

colorFind :: String -> [(Int, String)] -> Int
colorFind color list
  | null match = 0
  | otherwise = fst $ head match
  where
    match = filter (\(_, c) -> c == color) list

parseRound :: Parsec.Parsec String () Round
parseRound = do
  colors <- Parsec.sepBy parseColor (Parsec.string ", ")
  return $ Round (colorFind "red" colors) (colorFind "green" colors) (colorFind "blue" colors)

parseGame :: Parsec.Parsec String () Game
parseGame = do
  Parsec.string "Game "
  id <- Parsec.many1 Parsec.digit
  Parsec.string ": "
  rounds <- Parsec.sepBy parseRound (Parsec.string "; ")
  return $ Game rounds (read id :: Int)

parseGames :: Parsec.Parsec String () [Game]
parseGames = Parsec.endBy parseGame (Parsec.string "\n")

parser :: String -> [Game]
parser input = case Parsec.parse parseGames "" input of
  Left e -> error $ show e
  Right r -> r

validGame :: Game -> Bool
validGame (Game rounds _) = all validRound rounds
  where
    validRound (Round red green blue) = red <= 12 && green <= 13 && blue <= 14

power :: Game -> Int
power (Game rounds _) = maxRed * maxGreen * maxBlue
  where
    maxRed = maximum $ map (\(Round red _ _) -> red) rounds
    maxGreen = maximum $ map (\(Round _ green _) -> green) rounds
    maxBlue = maximum $ map (\(Round _ _ blue) -> blue) rounds

main = do
  contents <- readFile "input"
  let games = parser contents
  let validGames = filter validGame games
  putStrLn $ "p1: " ++ show (sum $ map (\(Game round id) -> id) validGames)
  putStrLn $ "p2: " ++ show (sum $ map power games)
