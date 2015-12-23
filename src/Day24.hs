module Day24 (day24
             ,day24'
             ,day24Input)
       where

import Data.List

day24Input :: IO [Int]
day24Input = (map read) . lines <$> readFile "inputs/day24"

packageOfSize' :: Int -> [Int] -> [Int] -> [[Int]]
packageOfSize' n [] accum = if length accum == n
                            then [accum]
                            else []
packageOfSize' n (x:xs) accum
  | len == n = [accum]
  | otherwise = (packageOfSize' n xs accum) ++ (packageOfSize' n xs (x:accum))
  where len = length accum

packageOfSize :: Int -> [Int] -> [[Int]]
packageOfSize n packages = packageOfSize' n packages []

findPackageOfSize' :: Int -> [Int] -> Int -> [[Int]]
findPackageOfSize' groupSize packages sizeTested = let p = filter (\l -> sum l == packageSize) (packageOfSize sizeTested packages)
                                                       packageSize = (sum packages `div` groupSize)
                                                   in if null p
                                                      then findPackageOfSize' groupSize packages (sizeTested + 1)
                                                      else p

findPackageOfSize :: Int -> [Int] -> [[Int]]
findPackageOfSize groupSize packages = findPackageOfSize' groupSize packages 0

day24 :: [Int] -> Int
day24 input = minimum (map product (findPackageOfSize 3 input))

day24' :: [Int] -> Int
day24' input = minimum (map product (findPackageOfSize 4 input))
