main :: IO ()
main = do
  input <- readFile "input"
  let grid = lines input

  putStrLn $ "p1: " ++ show (p1 grid)

p1 :: [String] -> Int
p1 grid = length $ filter (== '#') $ concat $ last $ take 1000 $ iterate step grid

step :: [String] -> [String]
step grid = [[next (x, y) | x <- [0 .. 99]] | y <- [0 .. 99]]
  where
    next (x, y) = case grid !! y !! x of
      '#' -> if surroundingOn grid (x, y) `elem` [2, 3] then '#' else '.'
      '.' -> if surroundingOn grid (x, y) == 3 then '#' else '.'

surroundingOn :: [String] -> (Int, Int) -> Int
surroundingOn grid (x, y) = length $ filter (== '#') $ map (\(x, y) -> grid !! y !! x) $ adjacent (x, y)

adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [(x + dx, y + dy) | dx <- [-1 .. 1], dy <- [-1 .. 1], dx /= 0 || dy /= 0, x + dx >= 0, y + dy >= 0, x + dx < 100, y + dy < 100]
