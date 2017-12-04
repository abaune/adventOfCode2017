module Day3 where

  import System.IO
  import Control.Monad
  import Data.List.Split

  main :: IO()
  main = do
    print $ getPart1 289326

  -- Needed a hint and after some googling, came accross this formula:
  -- layer(n) = ceil ( (sqrt n) - 1) / 2)
  -- upperright(L) = 2*L^2 - 2L + 1
  -- distance(n) = layer(n) + abs( remainder(n - upperright(layer(n)), layer(n))
  getPart1 :: Int -> Int
  getPart1 x = f + abs ((x - g) `mod` f) where
    f = ceiling $ (sqrt n - 1) / 2
    g = 2 * f^2 - 2 * f + 1
    n = fromIntegral x
