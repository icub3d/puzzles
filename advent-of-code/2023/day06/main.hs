import Data.Text.Lazy (pack, replace, unpack)
import qualified Text.Parsec as Parsec

-- Time:      7  15   30
-- Distance:  9  40  200
parseInputLine :: Parsec.Parsec String () [Int]
parseInputLine = do
  Parsec.many1 Parsec.alphaNum
  Parsec.char ':'
  Parsec.spaces
  ints <- Parsec.many1 Parsec.digit `Parsec.sepBy` Parsec.spaces
  return $ map read ints

parser :: String -> [Int]
parser input = case Parsec.parse parseInputLine "" input of
  Left e -> error $ show e
  Right r -> r

calcDistance :: Int -> Int -> Int
calcDistance total time = (total - time) * time

winner :: Int -> Int -> Int -> Int
winner distance totalTime time = if calcDistance totalTime time > distance then 1 else 0

winners :: (Int, Int) -> Int
winners (time, distance) = sum $ map (winner distance time) [0 .. time]

winnerTo :: Int -> Int -> Int
winnerTo time distance = length $ takeWhile (< distance) $ map (calcDistance time) [0 ..]

winnerFrom :: Int -> Int -> Int
winnerFrom time distance = length $ takeWhile (< distance) $ map (calcDistance time) (reverse [0 .. time])

winnersFaster :: (Int, Int) -> Int
winnersFaster (time, distance) = time - (winnerTo time distance + winnerFrom time distance) + 1

main :: IO ()
main = do
  input <- readFile "input"
  l <- return $ map parser $ lines input
  let zipped = zip (head l) (last l)
  putStrLn $ "p1: " ++ show (product $ map winners zipped)
  putStrLn $ "p1 (faster): " ++ show (product $ map winnersFaster zipped)

  let p2Input = unpack $ replace (pack " ") (pack "") (pack input)
  l <- return $ map parser $ lines p2Input
  let zipped = zip (head l) (last l)
  putStrLn $ "p2: " ++ show (product $ map winnersFaster zipped)
