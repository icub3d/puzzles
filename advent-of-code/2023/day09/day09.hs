import Data.List (tails)

windows :: Int -> [a] -> [[a]]
windows n = filter (\a -> length a == n) . foldr (zipWith (:)) (repeat []) . take n . tails

diffs :: [Int] -> [Int]
diffs = map (\a -> last a - head a) . windows 2
