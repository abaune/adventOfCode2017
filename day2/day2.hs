module Day2 where

  import System.IO
  import Control.Monad
  import Data.List.Split

  main :: IO()
  main = do
    list <- readFile "day2Puzzle.txt"
    print $ solve1 $ getMaxAndMin $ toNum $ removeTabs $ lines list
    print $ solve2 $flatten $ getTuples $ toNum $ removeTabs $ lines list

  flatten :: [[(Int, Int)]] -> [(Int, Int)]
  flatten xs = foldr (\x y -> x ++ y) [] xs

  solve2 :: [(Int, Int)] -> Int
  solve2 xs = foldr (\x y -> (fst x `div` snd x) + y) 0 xs

  getTuples :: [[Int]] -> [[(Int, Int)]]
  getTuples (x:xs) = getEvenlyDivisible x : getTuples xs
  getTuples [] = []

  getEvenlyDivisible :: [Int] -> [(Int, Int)]
  getEvenlyDivisible xs = [(x,y) | x <- xs, y <- xs, rem x y == 0, x /= y]

  solve1 :: [(Int, Int)] -> Int
  solve1 xs = foldr (\x y -> (fst x - snd x) + y) 0 xs

  toNum :: [[String]] -> [[Int]]
  toNum (x:xs) = (map (\y -> read y) x) : toNum xs
  toNum [] = []

  getMaxAndMin :: [[Int]] -> [(Int, Int)]
  getMaxAndMin xs = map (\x -> (getMax x, getMin x)) xs

  getMax :: [Int] -> Int
  getMax xs = foldr max 0 xs

  getMin :: [Int] -> Int
  getMin (x:xs) = foldr min x xs

  removeTabs :: [String] -> [[String]]
  removeTabs (x:xs) = (splitOn "\t" x) : removeTabs xs
  removeTabs [] = []
