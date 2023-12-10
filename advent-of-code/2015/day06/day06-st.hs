{-# LANGUAGE BlockArguments #-}

import Control.Monad (forM_)
import Control.Monad.ST (ST, runST)
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as UArray
import Data.List (foldl', stripPrefix)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as M

data Point = Point {x :: Int, y :: Int} deriving (Show, Eq, Ord, UArray.Ix)

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

pointToIndex :: Point -> Int
pointToIndex (Point x y) = x + y * 1000

expand :: Instruction -> [Point]
expand (Instruction _ (Point x1 y1) (Point x2 y2)) = [Point x y | x <- [x1 .. x2], y <- [y1 .. y2]]

apply :: Mutator -> [Instruction] -> V.Vector Int
apply mutator instructions = V.create do
  grid <- M.new (1000 * 1000)
  M.set grid 0
  forM_ instructions \instruction -> do
    forM_ (expand instruction) \point -> do
      let index = pointToIndex point
      M.unsafeModify grid (mutator (operation instruction)) (pointToIndex point)
  pure grid

p1 :: [Instruction] -> Int
p1 instructions = V.sum $ apply english instructions

p2 :: [Instruction] -> Int
p2 instructions = V.sum $ apply elvish instructions

main :: IO ()
main = do
  input <- map parseInstruction . lines <$> readFile "input"
  putStrLn $ "p1: " ++ show (p1 input)
  putStrLn $ "p2: " ++ show (p2 input)
