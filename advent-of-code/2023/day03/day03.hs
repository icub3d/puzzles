import Data.Char (isDigit)
import Data.Set (Set)

import Control.Monad (lift)
import qualified Control.Monad.State as State

import qualified Text.Parsec as Parsec

gridSize = 140

grid = ".4.*\n...1"

-- Use Parsec to parse the grid keeping track of the current position.
parseGrid :: Parsec.ParsecT String () (S.State (Int, Int)) ()
parseGrid = do


-- Get the adjacent coordinates of a given coordinate while staying
-- within the grid
adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], (dx, dy) /= (0, 0), x + dx >= 0, x + dx < gridSize, y + dy >= 0, y + dy < gridSize]

isSymbol :: Char -> Bool
isSymbol c = (not . isDigit) c && c /= '.'

adjacentToSymbol :: [[Char]] -> (Int, Int) -> Bool
adjacentToSymbol grid (x, y) = any (isSymbol . (\(x, y) -> grid !! y !! x)) $ adjacent (x, y)
