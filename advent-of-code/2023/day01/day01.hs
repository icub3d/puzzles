import Data.Bifunctor (bimap)
import Data.Char (isDigit)
import Data.List (concat, foldl', isPrefixOf, tails)
import Data.Maybe (catMaybes)
import Data.Text.Lazy (pack, replace, unpack)

main :: IO ()
main = do
  input <- readFile "input"
  let calibrations = lines input
  putStrLn $ "p1:    " ++ show (part1 calibrations (filter isDigit))
  putStrLn $ "p2:    " ++ show (part1 calibrations p2filter)
  putStrLn $ "p2(b): " ++ show (part1 (map replaceStringDigits calibrations) $ filter isDigit)

part1 :: [String] -> (String -> String) -> Int
part1 calibrations filterer = sum $ map (number . filterer) calibrations
  where
    number dd = read [head dd, last dd] :: Int

p2filter :: String -> String
p2filter = concatMap show . catMaybes . firstDigits

firstDigits :: String -> [Maybe Int]
firstDigits = map firstDigit . tails

firstDigit :: String -> Maybe Int
firstDigit [] = Nothing
firstDigit all@(x : xs)
  | isDigit x = Just (read [x] :: Int)
  | "one" `isPrefixOf` all = Just 1
  | "two" `isPrefixOf` all = Just 2
  | "three" `isPrefixOf` all = Just 3
  | "four" `isPrefixOf` all = Just 4
  | "five" `isPrefixOf` all = Just 5
  | "six" `isPrefixOf` all = Just 6
  | "seven" `isPrefixOf` all = Just 7
  | "eight" `isPrefixOf` all = Just 8
  | "nine" `isPrefixOf` all = Just 9
  | otherwise = Nothing

chunks :: String -> [String]
chunks [] = []
chunks all@(x : xs) = all : chunks xs

replaceStringDigits :: String -> String
replaceStringDigits haystack = unpack $ foldl' (\h (replacement, needle) -> replace needle replacement h) (pack haystack) replacements
  where
    replacements =
      map
        (bimap pack pack)
        [ ("o1e", "one"),
          ("t2o", "two"),
          ("t3e", "three"),
          ("f4r", "four"),
          ("f5e", "five"),
          ("s6x", "six"),
          ("s7n", "seven"),
          ("e8t", "eight"),
          ("n9e", "nine")
        ]
