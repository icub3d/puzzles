import Data.List (delete)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $ "p1: " ++ show (part1 input)
  putStrLn $ "p2: " ++ show (part2 input)

splitToTuple :: [[Char]] -> (Integer, Integer, Integer)
splitToTuple [a, b, c] = (read a, read b, read c)
splitToTuple _ = undefined

p :: String -> (Integer, Integer, Integer)
p s = splitToTuple $ splitOn "x" s

sa :: (Integer, Integer, Integer) -> Integer
sa (l, w, h) = 2 * s1 + 2 * s2 + 2 * s3 + minimum [s1, s2, s3]
  where
    s1 = l * w
    s2 = w * h
    s3 = h * l

ribbon :: (Integer, Integer, Integer) -> Integer
ribbon (l, w, h) = m1 + m1 + m2 + m2 + (l * w * h)
  where
    m1 = minimum [l, w, h]
    m2 = minimum $ delete m1 [l, w, h]

part1 :: String -> Integer
part1 input = sum $ map (sa . p) $ lines input

part2 :: String -> Integer
part2 input = sum $ map (ribbon . p) $ lines input
