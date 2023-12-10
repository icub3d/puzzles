import Data.List (foldl1, isSuffixOf)
import Data.Map (Map, fromList, keys, (!))
import Text.Parsec (Parsec, endBy, letter, many1, oneOf, parse, string)

data Edge = Edge {left :: String, right :: String} deriving (Show)

parseEdge :: Parsec String () (String, Edge)
parseEdge = do
  key <- many1 letter
  string " = ("
  left <- many1 letter
  string ", "
  right <- many1 letter
  string ")"
  return (key, Edge left right)

data Direction = L | R deriving (Show)

parseDirection :: Char -> Direction
parseDirection 'L' = L
parseDirection _ = R

data Instructions = Instructions {directions :: [Direction], edges :: Map String Edge} deriving (Show)

parseInstructions :: Parsec String () Instructions
parseInstructions = do
  directions <- many1 (oneOf "LR")
  string "\n\n"
  edges <- endBy parseEdge (string "\n")
  return $ Instructions (map parseDirection directions) (fromList edges)

next :: Map String Edge -> String -> Direction -> String
next edges current L = left $ edges ! current
next edges current R = right $ edges ! current

p1 :: Instructions -> (String -> Bool) -> String -> Int
p1 (Instructions directions edges) endFn start = length . takeWhile endFn $ scanl (next edges) start (cycle directions)

p2 :: Instructions -> Int
p2 (Instructions directions edges) = foldl1 lcm $ map len starts
  where
    starts = filter (isSuffixOf "A") $ keys edges
    len = p1 (Instructions directions edges) (not . isSuffixOf "Z")

main = do
  input <- readFile "input"
  let instructions = case parse parseInstructions "" input of
        Left err -> error $ show err
        Right instructions -> instructions
  putStrLn $ "p1: " ++ show (p1 instructions (/= "ZZZ") "AAA")
  putStrLn $ "p2: " ++ show (p2 instructions)
