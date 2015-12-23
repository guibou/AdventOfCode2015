{-# LANGUAGE TupleSections #-}

module Day25 where

day25Input :: (Int, Int)
day25Input = (2947, 3029)

firstCode :: Integer
firstCode = 20151125

mul :: Integer
mul = 252533

modulus :: Integer
modulus = 33554393

-- Unused (Slow, for testing only)

nextCode :: Integer -> Integer
nextCode code = (code * mul) `mod` modulus

nextCodeN :: Int -> Integer
nextCodeN n = head $ drop n (iterate nextCode firstCode)

codeAtSlow :: (Integer, Integer) -> Integer
codeAtSlow (1, 1) = firstCode
codeAtSlow (row, 1) = nextCode (codeAtSlow (1, row - 1))
codeAtSlow (row, column) = nextCode (codeAtSlow (row + 1, column - 1))

-- Quick

nextCodeN' :: Int -> Integer
nextCodeN' n = (firstCode * (mul ^ n)) `mod` modulus

codeCount :: (Int, Int) -> Int
codeCount (row, column) = (column - 1) + (n * (n + 1)) `div` 2
  where n = row + column - 2

codeAt :: (Int, Int) -> Integer
codeAt pos = nextCodeN' (codeCount pos)

-- display

grid :: Show t => ((Integer, Integer) -> t) -> String
grid f = unlines (map line [1..6])
  where line l = (unwords . (map show)) (map (f . (l, )) [1..6])

day25 :: Integer
day25 = codeAt day25Input
