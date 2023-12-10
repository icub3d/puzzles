import Data.List (isInfixOf)

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $ "p1: " ++ show (part1 (lines input))
  putStrLn $ "p2: " ++ show (part2 (lines input))

w :: Int -> [a] -> [[a]]
w _ [] = []
w n xs@(x : xs')
  | length xs < n = []
  | otherwise = take n xs : w n xs'

pair :: String -> Bool
pair [] = False
pair [_] = False
pair (x : y : xs) = [x, y] `isInfixOf` xs || pair (y : xs)

nice' :: String -> Int
nice' s = if pair s && repeat s then 1 else 0
  where
    repeat s = any (\[a, b, c] -> a == c) (w 3 s)

nice :: String -> Int
nice s = if vowels s && double s && not (bad s) then 1 else 0
  where
    vowels s = length (filter (`elem` "aeiou") s) >= 3
    double s = any (uncurry (==)) (zip s (tail s))
    bad s = any (`isInfixOf` s) ["ab", "cd", "pq", "xy"]

part1 :: [String] -> Int
part1 lines = sum $ map nice lines

part2 :: [String] -> Int
part2 lines = sum $ map nice' lines
