import Data.List (foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

parsePoint :: String -> Point
parsePoint s = Point (read x) (read y)
  where
    [x, y] = splitOn "," s

data Range = Range {start :: Point, end :: Point} deriving (Show, Eq)

inRange :: Range -> Point -> Bool
inRange (Range (Point sx sy) (Point ex ey)) (Point x y) = x >= sx && x <= ex && y >= sy && y <= ey

data Operation = TurnOn | TurnOff | Toggle deriving (Enum, Show, Eq)

parseOperation :: String -> Operation
parseOperation "on" = TurnOn
parseOperation "off" = TurnOff
parseOperation "toggle" = Toggle
parseOperation _ = error "Invalid operation"

data Instruction = Instruction {operation :: Operation, range :: Range} deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s = Instruction (parseOperation op) (Range (parsePoint start) (parsePoint end))
  where
    [op, start, _, end] = splitOn " " $ fromMaybe s (stripPrefix "turn " s)

type Mutator = Operation -> Int -> Int

english :: Operation -> Int -> Int
english TurnOn i = 1
english TurnOff i = 0
english Toggle i = 1 - i

elvish :: Operation -> Int -> Int
elvish TurnOn i = i + 1
elvish TurnOff i = max 0 (i - 1)
elvish Toggle i = i + 2

apply :: Point -> Mutator -> Int -> Instruction -> Int
apply p m v (Instruction op r) = if inRange r p then m op v else v

applyAll :: Mutator -> [Instruction] -> Point -> Int
applyAll m instructions p = foldl' (apply p m) 0 instructions

p1 :: [Instruction] -> Int
p1 instructions = sum [applyAll english instructions $ Point x y | x <- [0 .. 999], y <- [0 .. 999]]

p2 :: [Instruction] -> Int
p2 instructions = sum [applyAll elvish instructions $ Point x y | x <- [0 .. 999], y <- [0 .. 999]]

main :: IO ()
main = do
  input <- map parseInstruction . lines <$> readFile "input"
  putStrLn $ "p1: " ++ show (p1 input)
  putStrLn $ "p2: " ++ show (p2 input)
