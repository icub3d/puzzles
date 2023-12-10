import Data.List (foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord)

parsePoint :: String -> Point
parsePoint s = Point (read x) (read y)
  where
    [x, y] = splitOn "," s

data Operation = TurnOn | TurnOff | Toggle deriving (Enum, Show, Eq)

parseOperation :: String -> Operation
parseOperation "on" = TurnOn
parseOperation "off" = TurnOff
parseOperation "toggle" = Toggle
parseOperation _ = error "Invalid operation"

data Instruction = Instruction {operation :: Operation, start :: Point, end :: Point} deriving (Show, Eq)

parseInstruction :: String -> Instruction
parseInstruction s = Instruction (parseOperation op) (parsePoint start) (parsePoint end)
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

type Grid = Map Point Int

apply :: Mutator -> Grid -> Instruction -> Grid
apply mutator grid (Instruction op (Point x1 y1) (Point x2 y2)) = foldl' apply' grid points
  where
    points = [Point x y | x <- [x1 .. x2], y <- [y1 .. y2]]
    apply' grid point = M.insert point (mutator op (fromMaybe 0 (M.lookup point grid))) grid

applyAll :: [Instruction] -> Mutator -> Grid
applyAll instructions mutator = foldl' (apply mutator) M.empty instructions

p2 instructions = M.foldr (+) 0 $ applyAll instructions elvish

p1 instructions = M.foldr (+) 0 $ applyAll instructions english

main = do
  input <- readFile "input"
  let instructions = map parseInstruction $ lines input
  putStrLn $ "p1: " ++ show (p1 instructions)
  putStrLn $ "p2: " ++ show (p2 instructions)
