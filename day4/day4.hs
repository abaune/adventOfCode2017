module Day4 where

  import System.IO
  import Control.Monad
  import Data.List

  main :: IO()
  main = do
    list <- readFile "day4.txt"
    print $ getPart1 $ lines list
    print $ getPart2 $ lines list

  getPart1 :: [String] -> Int
  getPart1 xs = foldr (\pwd accum -> accum + validate (words pwd)) 0 xs

  validate :: [String] -> Int
  validate (x:xs)
    | x `elem` xs = 0
    | otherwise = validate xs
  validate [] = 1

  getPart2 :: [String] -> Int
  getPart2 xs = foldr (\pwd accum -> accum + (validate . sortVals) (words pwd)) 0 xs

  sortVals :: [String] -> [String]
  sortVals xs = map sort xs
