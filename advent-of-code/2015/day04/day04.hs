{-# LANGUAGE PackageImports #-}

import "cryptonite" Crypto.Hash
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.UTF8 as B
import Data.List (isPrefixOf)
import System.Environment

h :: String -> Int -> String
h input count = show (hashlazy (B.fromString s) :: Digest MD5)
  where
    s = input ++ show count

solve :: String -> Int -> String -> Int
solve input acc prefix
  | prefix `isPrefixOf` h input acc = acc
  | otherwise = solve input (acc + 1) prefix

main :: IO ()
main = do
  let input = "bgvyzdsv"
  print $ solve input 1 "00000"
  print $ solve input 1 "000000"
