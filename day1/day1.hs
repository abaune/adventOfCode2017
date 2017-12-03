module Day1 where

  import System.IO
  import Control.Monad

  x = readFile "part1Puzzle.txt"

  main :: IO()
  main = do
    list <- readFile "day1Puzzle.txt"
    let listWithoutNewLine = (reverse . tail . reverse) list
    print $ getAnswer listWithoutNewLine
    print $ sum $ p2ToNum $ getPart2 list list 0 (length listWithoutNewLine) []

  getAnswer :: String -> Int
  getAnswer xs = sum $ toNum $ getDup $ getTuples xs xs

  toNum :: [(Char, Char)] -> [Int]
  toNum xs = map (\x -> read $ (fst x) : []) xs

  getDup :: [(Char, Char)] -> [(Char, Char)]
  getDup xs = filter (\x -> fst x == snd x) xs

  getTuples :: String -> String -> [(Char, Char)]
  getTuples x (y:ys) = zip x (ys ++ y : [])

  p2ToNum :: String -> [Int]
  p2ToNum xs = map (\x -> read $ x : []) xs

  getListItem :: String -> String -> Int -> Int -> Char
  getListItem originalList (x:xs) i listLength = originalList !! (((listLength `div` 2) + i) `mod` listLength)

  getPart2 :: String -> String -> Int -> Int -> String -> String
  getPart2 originalList (x:xs) i listLength accum
    | (xs == [] && x == getListItem originalList (x:xs) i listLength ) = accum ++ [x]
    | xs == [] = accum
    | x == getListItem originalList (x:xs) i listLength = getPart2 originalList xs (i + 1) listLength (accum ++ [x])
    | otherwise = getPart2 originalList xs (i + 1) listLength accum
