main :: IO ()
main = do
  input <- readFile "input"
  putStrLn $ "p1: " ++ show (part1 input)
  putStrLn $ "p2: " ++ show (part2 input)

m :: Char -> Int
m c = case c of
  '(' -> 1
  ')' -> -1
  _ -> 0

part1 :: String -> Int
part1 = sum . map m

part2 :: String -> Int
part2 = length . takeWhile (>= 0) . scanl (+) 0 . map m
