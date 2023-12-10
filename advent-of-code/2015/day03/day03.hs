import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $ "p1: " ++ show (length (part1 input))
  putStrLn $ "p2: " ++ show (length (part2 input))

type Point = (Integer, Integer)

move :: Char -> Point -> Point
move '^' (x, y) = (x, y + 1)
move 'v' (x, y) = (x, y - 1)
move '>' (x, y) = (x + 1, y)
move '<' (x, y) = (x - 1, y)
move _ _ = error "Invalid move"

split :: [a] -> ([a], [a])
split [] = ([], [])
split [x] = ([x], [])
split (x : y : xs) = (x : xs', y : ys')
  where
    (xs', ys') = split xs

part1 :: String -> Set.Set Point
part1 input = helper input (0, 0) (Set.insert (0, 0) Set.empty)
  where
    helper [] _ set = set
    helper (x : xs) cur set = helper xs (move x cur) (Set.insert cur set)

part2 :: String -> Set.Set Point
part2 input = Set.union (part1 santa) (part1 robot)
  where
    (santa, robot) = split input
