{-# LANGUAGE TemplateHaskell #-}

module Days1to21 where

import Data.List (isInfixOf, sort, isPrefixOf, isSuffixOf, nub, permutations, minimumBy, maximumBy, group, delete, groupBy, sortBy, find)
import Data.Foldable (foldl')

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import qualified Data.Vector as VM
import qualified Data.Vector.Unboxed as VU
import Data.Hash.MD5 as MD5

import Data.Function.Memoize (memoFix, deriveMemoizable, memoFix2)
import Data.Hex (unhex)
import Data.List.Split (splitOn)
import Data.Scientific (Scientific)
import Data.Bits (shiftR, shiftL, complement, (.&.), (.|.))
import Data.Maybe (fromJust, catMaybes)
import Control.Monad (guard)

import System.Environment (getArgs)
import Debug.Trace

import Data.Function (fix)
import GHC.Exts (sortWith)

import Control.Monad (replicateM)

import Text.Parsec
import Data.Word

-- Day 1

dayAdd :: Char -> Int
dayAdd '(' = 1
dayAdd ')' = -1
dayAdd _ = 0

day1Input :: IO String
day1Input = readFile "inputs/day1"

day1 :: String -> Int
day1 content = sum (map dayAdd content)

day1' :: String -> Int
day1' content = length (takeWhile (>=0) (scanl (+) 0 (map dayAdd content)))

-- Day 2

day2Input :: IO ([(Int, Int, Int)])
day2Input = do
  content <- readFile "inputs/day2"
  let ret = map (splitOn "x") (lines content)
  return $ map (\[x, y, z] -> (read x, read y, read z)) ret

wrappingSize :: (Int, Int, Int) -> Int
wrappingSize (x, y, z) = let l = [x * y, x * z, y * z]
                         in 2 * sum l + minimum l

ribbonSize :: (Int, Int, Int) -> Int
ribbonSize (x, y, z) = let l = sort [x, y, z]
                       in 2 * sum (take 2 l) + product l

day2 items = (sum . (map wrappingSize)) items

day2' items = (sum . (map ribbonSize)) items

-- Day 3

day3Input :: IO [(Int, Int)]
day3Input = do
  content <- readFile "inputs/day3"
  return $ map toMove content
    where
      toMove '^' = (0, -1)
      toMove '>' = (1, 0)
      toMove '<' = (-1, 0)
      toMove 'v' = (0, 1)
      toMove _ = (0, 0) -- I may have crap in the input

move :: (Float, Float) -> (Float, Float) -> (Float, Float)
move (x, y) (dx, dy) = (x + dx, y + dy)

day3 moves = length (nub (scanl move (0, 0) moves))

takeOneOverTwo [] = []
takeOneOverTwo (x:xs) = x:takeOneOverTwo (drop 1 xs)

day3' moves = length . nub $ (scanl move (0, 0) (takeOneOverTwo moves)) ++ (scanl move (0, 0) (takeOneOverTwo (drop 1 moves)))

-- Day 4

day4Input :: IO String
day4Input = return $ "yzbqklnj"

day4'' size input = length $ takeWhile (not . ((replicate size '0')`isPrefixOf`)) (map (\x -> MD5.md5s (MD5.Str (input ++ (show x)))) [0..])

day4 = day4'' 5
day4' = day4'' 6

-- Day 5

goodWord word = not (any (`isInfixOf` word) ["ab", "cd", "pq", "xy"])
                && any (`isInfixOf` word) (map (replicate 2) ['a'..'z'])
                && length (filter (`elem` "aeiou") word) >= 3


oneRepeatWithOneInTheMiddle (a:b:c:xs)
  | a == c = True
  | otherwise = oneRepeatWithOneInTheMiddle (b:c:xs)
oneRepeatWithOneInTheMiddle _ = False

pairTwice (a:b:xs)
  | [a, b] `isInfixOf` xs = True
  | otherwise = pairTwice (b:xs)
pairTwice _ = False

goodWord' word = oneRepeatWithOneInTheMiddle word && pairTwice word

day5Input = words <$> readFile "inputs/day5"

day5 words = length (filter goodWord words)

day5' words = length (filter goodWord' words)

-- Day 6

data Range = Range Int Int deriving (Show)
data Action = On Range Range | Off Range Range | Toggle Range Range deriving (Show)

parseToggle = string "toggle" *> return Toggle
parseOn = string "turn on" *> return On
parseOff = string "turn off" *> return Off

decimal = read <$> (many1 (oneOf "0123456789"))

parseRange = Range <$> decimal <*> (char ',' *> decimal)

parseCommand = do
  cmd <- choice [try parseToggle, try parseOn, parseOff]
  space
  r0 <- parseRange
  string " through "
  r1 <- parseRange

  return $ cmd r0 r1

parseCommands = parseCommand `sepBy` (string "\n")

day6Input = do
  input <- readFile "inputs/day6"
  return $ let (Right res) = parse parseCommands "BLORK" input in res

range :: Bool -> Range -> Range -> [(Int, Bool)]
range val (Range x0 y0) (Range x1 y1) = do
  y <- [y0..y1]
  x <- [x0..x1]

  return $ (x + y * 1000, val)
  
day6 :: [Action] -> Int
day6 input = let v = V.replicate (1000 * 1000) False in sum (V.map (\x -> if x then 1 else 0) (foldl' acc v input))
  where
    acc :: V.Vector Bool -> Action -> V.Vector Bool
    acc v (On r0 r1) = v V.// (range True r0 r1)
    acc v (Off r0 r1) = v V.// (range False r0 r1)
    acc v (Toggle r0 r1) = V.accumulate toggle v (V.fromList (range False r0 r1))

    toggle v0 _ = not v0
  
day6' :: [Action] -> Int
day6' input = let v = V.replicate (1000 * 1000) 0 in sum (foldl' acc v input)
  where
    acc :: V.Vector Int -> Action -> V.Vector Int
    acc v (On r0 r1) = V.accumulate (\x _ -> x + 1) v (V.fromList (range False r0 r1))
    acc v (Off r0 r1) = V.accumulate (\x _ -> max 0 (x - 1)) v (V.fromList (range False r0 r1))
    acc v (Toggle r0 r1) = V.accumulate (\x _ -> x + 2) v (V.fromList (range False r0 r1))

-- Day 7

-- Data definitions
data Line = Line Expr String deriving (Show)
data Expr = Lit Int
          | Gate String
          | And Expr Expr
          | Not Expr
          | Rshift Expr Expr
          | Lshift Expr Expr
          | Or Expr Expr deriving (Show)

-- Literals
parseLit = Lit <$> decimal
parseGate = Gate <$> many1 (oneOf ['a'..'z'])

parseGateOrLiteral = choice [parseLit, parseGate]

-- Binops
parseBinop s ctor = ctor <$> (parseGateOrLiteral <* string (" " ++ s ++ " "))
                    <*> parseGateOrLiteral

parseAnd = parseBinop "AND" And
parseOr = parseBinop "OR" Or
parseRshift = parseBinop "RSHIFT" Rshift
parseLshift = parseBinop "LSHIFT" Lshift
parseNot = Not <$> (string "NOT " *> parseGateOrLiteral)

-- Line (i.e an expression -> a gate)
parseLine = do
  expr <- choice (map try [parseNot, parseAnd, parseOr, parseRshift, parseLshift, parseGateOrLiteral])
  _ <- string " -> "
  Gate output <- parseGate

  return $ Line expr output

-- All lines
parseLines = parseLine `sepBy` (string "\n")

deriveMemoizable ''Expr

day7Input = do
  input <- readFile "inputs/day7"
  return $ let (Right res) = parse parseLines "BLORK" input in res

eval :: H.HashMap String Expr -> Expr -> Int
eval commands v = go' v
  where
    go' = memoFix go

    go go'' (Lit i) = i
    go go'' (Gate s) = go'' (fromJust (H.lookup s commands))
    go go'' (And expr0 expr1) = (go'' expr0) .&. (go'' expr1)
    go go'' (Or expr0 expr1) = (go'' expr0)  .|. (go'' expr1)
    go go'' (Rshift expr0 expr1) = (go'' expr0) `shiftR` (go'' expr1)
    go go'' (Lshift expr0 expr1) = (go'' expr0) `shiftL` (go'' expr1)
    go go'' (Not expr) = complement (go'' expr)

day7 lines = let commands = H.fromList (map (\(Line e name) -> (name, e)) lines)
             in eval commands (Gate "a")

day7' lines = let commands = H.fromList (map (\(Line e name) -> (name, e)) lines)
                  retb = eval commands (Gate "a")
                  commands' = H.insert "b" (Lit retb) commands
              in eval commands' (Gate "a")

dispItem (Lit i) = show i
dispItem (Gate s) = s
dispItem (And expr0 expr1) = dispItem expr0 ++ " AND " ++ dispItem expr1
dispItem (Or expr0 expr1) = dispItem expr0 ++ " OR " ++ dispItem expr1
dispItem (Rshift expr0 expr1) = dispItem expr0 ++ " RSHIFT " ++ dispItem expr1
dispItem (Lshift expr0 expr1) = dispItem expr0 ++ " LSHIFT " ++ dispItem expr1
dispItem (Not expr) = "NOT " ++ dispItem expr

dispLine (Line e s) = dispItem e ++ " -> " ++ s

dispLines lines = unlines (map dispLine lines)

-- day 8

parseHexa :: Parsec String () Char
parseHexa = do
  string "\\x"
  c0 <- anyChar
  c1 <- anyChar

  let (c:[]) = unhex (c0:c1:[])
  return (head c)

parseChar = try $ choice [
  try (string "\\\\" *> return '\\'),
  try (string "\\\"" *> return '"'),
  try parseHexa,
  try anyChar]
parseLine8 = string "\"" *> (manyTill parseChar (string "\""))

convertLine :: String -> String
convertLine s = let (Right res) = parse parseLine8 "BLORK" s
                in res

encodeChar '\\' = "\\\\"
encodeChar '\"' = "\\\""
encodeChar n = [n]

encodeLine s = "\"" ++ (mconcat (map encodeChar s)) ++ "\""
                   
day8Input = words <$> readFile "inputs/day8"

day8 input = sum (map (\(x, y) -> length x - length y) (zip input (map convertLine input)))

day8' input = sum (map (\(x, y) -> length y - length x) (zip input (map encodeLine input)))

-- day 9

day9Input = do
  content <- lines <$> readFile "inputs/day9"
  
  let p = (map (\x -> let [f, _, t, _, d] = words x
                      in (f, t, read d)) content) :: [(String, String, Int)]

      paths = concat (map (\(f, t, d) -> [(f, H.fromList [(t, d)]), (t, H.fromList [(f, d)])]) p)
  return $ H.fromListWith merge paths
  where
    merge h1 h2 = H.union h1 h2

minimumWith :: Ord t' => [t] -> (t -> t') -> t
minimumWith l f = snd (minimumBy (\(x, _) (y, _) -> x `compare` y) (map (\x -> (f x, x)) l ))

maximumWith :: Ord t' => [t] -> (t -> t') -> t
maximumWith l f = snd (maximumBy (\(x, _) (y, _) -> x `compare` y) (map (\x -> (f x, x)) l ))

pathLength graph (x:y:xs) = pathLength graph (y:xs) + (fromJust (H.lookup y (fromJust (H.lookup x graph))))
pathLength graph [_] = 0

day9 graph = let perms = permutations (H.keys graph)
                 path = minimumWith perms (pathLength graph)
             in (path, pathLength graph path)

day9' graph = let perms = permutations (H.keys graph)
                  path = maximumWith perms (pathLength graph)
              in (path, pathLength graph path)


-- day10

day10Input :: IO String
day10Input = return "1113122113"

lookAndSay :: String -> String
lookAndSay s = let g = group s
               in mconcat (map (\l -> (show $ length l) ++ [head l]) g)

day10 s = length (head (drop 50 (iterate lookAndSay s)))

-- day11
day11Input :: IO String
day11Input = return "hepxcrrq"

inc' (x:xs) = let x' = succ x
             in if x' == succ 'z'
                then 'a':inc xs
                else x':xs
                     
inc input = reverse (inc' (reverse input))

getOverlapCount (x:y:xs)
  | x == y = (x:y:[]):getOverlapCount xs
  | otherwise = getOverlapCount (y:xs)
getOverlapCount _ = []

hasOneIncreasingOverlap (x:y:z:xs)
  | succ x == y && succ y == z = True
  | otherwise = hasOneIncreasingOverlap (y:z:xs)
hasOneIncreasingOverlap _ = False

okPassword password = not (any (`elem`"iol") password)
                      && length (nub (getOverlapCount password)) >= 2
                      && hasOneIncreasingOverlap password


increment input = let input' = inc input
                  in if okPassword input'
                     then input'
                     else increment input'

day11 input = increment input

-- day 12

day12Input = do
  content <- B.readFile "inputs/day12"
  return $ fromJust ((A.decode content) :: Maybe A.Value)

day12a :: (A.Value -> Scientific) -> A.Value -> Scientific
day12a f (A.Array v) = foldl' (+) 0 (fmap f v)
day12a _ (A.String _) = 0
day12a _ (A.Number n) = n
day12a _ (A.Bool _) = 0
day12a _ (A.Null) = 0
day12a f (A.Object h) = foldl' (+) 0 (fmap f (H.elems h))

day12 = day12a day12

day12' (A.Object h) = let elems = H.elems h
                          red = A.String (T.pack "red")
                      in if red `elem` elems
                         then 0
                         else foldl' (+) 0 (fmap day12' (H.elems h))
day12' v = day12a day12' v

-- day 13

toHappyness line = let [name, _, action, value, _, _, _, _, _, _, other] = words line
                       value' = read value
                       other' = take (length other -1) other
                   in (name, H.fromList [(other', if action == "gain" then value' else (-value'))])

day13Input :: IO (H.HashMap String (H.HashMap String Int))
day13Input = do
  content <- lines <$> readFile "inputs/day13"
  return $ H.fromListWith (H.union) $ map toHappyness content

getWeight _ "Me" _ = 0
getWeight _ _ "Me" = 0
getWeight graph x y = (fromJust (H.lookup y (fromJust (H.lookup x graph)))) + 
                      (fromJust (H.lookup x (fromJust (H.lookup y graph))))

tableWeight graph table = let cyclictable = table ++ [head table]
                          in computeWeight cyclictable
  where
    computeWeight :: [String] -> Int
    computeWeight [x] = 0
    computeWeight (x:y:xs) = (getWeight graph x y) + computeWeight (y:xs)

addMe :: H.HashMap String (H.HashMap String Int) -> H.HashMap String (H.HashMap String Int)
addMe graph = let k = H.keys graph
              in H.insert "Me" (H.empty) graph


day13 input = let names = H.keys input
                  tables = permutations names
                  m = maximumWith tables (tableWeight input)
              in (m, tableWeight input m)

day13' input = day13 (addMe input)

-- day 14

readReinderSpeed :: String -> (String, Int, Int, Int)
readReinderSpeed l = let [name, _, _, speed, _, _, timeRun, _, _, _, _,_ ,_ , timeRest, _] = words l
                     in (name, read speed, read timeRun, read timeRest)

reinderDistance :: Int -> (String, Int, Int, Int) -> Int
reinderDistance time (_, speed, timeRun, timeRest) = nbRound * speed * timeRun + (min lastRound timeRun) * speed
  where roundTime = timeRun + timeRest
        (nbRound, lastRound) = divMod time roundTime

day14Input = do
  content <- (map readReinderSpeed) <$> (lines <$> readFile "inputs/day14")
  return content

allMaximumWith :: Ord b => (a -> b) -> [a] -> [a]
allMaximumWith f l = let m = maximumWith l f
                     in filter (\x -> (f x == f m)) l

day14'' :: Int -> [(String, Int, Int, Int)] -> (String, Int, Int, Int)
day14'' t input = let reinder@(name, _, _, _) = maximumWith input (reinderDistance t)
              in reinder

day14 input = let reinder = day14'' 2503 input
              in reinderDistance 2503 reinder

type Reinder = (String, Int, Int, Int)

winnersOfOneRun :: [Reinder] -> Int -> [Reinder]
winnersOfOneRun items t = allMaximumWith (\i -> reinderDistance t i) items

day14' input = let
  allRuns :: [Reinder]
  allRuns = mconcat $ map (winnersOfOneRun input) [1..2503]
  winners = sort (map (\(name, _, _, _) -> name) allRuns)
  groups = group winners
  in maximum (map (\l -> (length l, head l)) groups)

-- day15

data Cookie = Cookie {capacity :: !Int, durability :: !Int, flavor :: !Int, texture :: !Int, calories :: !Int} deriving (Show)


parseCookie :: String -> Cookie
parseCookie input = let [_, _, c, _, d, _, f, _, t, _, ca] = words input
                    in Cookie (rd c) (rd d) (rd f) (rd t) (read ca)
  where rd s = read (take ((length s) - 1) s)

teaspoonValues :: [[Int]]
teaspoonValues = do
  a <- [0..100]
  b <- [0..(100 - a)]
  c <- [0..(100 - (a + b))]
  return [a, b, c, (100 - a - b - c)]

applyCookieWeight :: (Cookie, Int) -> Cookie
applyCookieWeight ((Cookie a b c d e), w) = Cookie (w * a) (w * b) (w * c) (w * d) (w * e)

sumCookie :: Cookie -> Cookie -> Cookie
sumCookie (Cookie a b c d e) (Cookie a' b' c' d' e') = Cookie (a + a') (b + b') (c + c') (d + d') (e + e')

productCookie :: Cookie -> Int
productCookie (Cookie a b c d cal) = if cal == 500
                                     then product (map (max 0) [a, b, c, d])
                                     else 0

sumWeight :: [Cookie] -> Int
sumWeight l = productCookie cookie
  where
    cookie :: Cookie
    cookie = foldl1 sumCookie l

cookieWeight :: [Cookie] -> [Int] -> Int
cookieWeight ingrediants weights = sumWeight (map applyCookieWeight (zip ingrediants weights))


day15Input = (map parseCookie) <$> (lines <$> readFile "inputs/day15")

day15 ingrediants = let m = maximumWith teaspoonValues (cookieWeight ingrediants)
                    in cookieWeight ingrediants m

-- day 16

type SueItems = H.HashMap String Int

data Sue = Sue Int SueItems deriving (Show)

parseSueItem = do
  name <- manyTill (anyChar) (string ": ")
  value <- decimal

  return (name, value)

parseSue = do
  n <- string "Sue " *> decimal <* string ": "
  items <- H.fromList <$> (parseSueItem `sepBy` (string ", "))

  return $ Sue n items

mySue :: SueItems
mySue = H.fromList [
  ("children", 3),
  ("cats", 7),
  ("samoyeds", 2),
  ("pomeranians", 3),
  ("akitas", 0),
  ("vizslas", 0),
  ("goldfish", 5),
  ("trees", 3),
  ("cars", 2),
  ("perfumes", 1)]


matchSueRequest :: Sue -> (String, Int) -> Bool
matchSueRequest (Sue _ items) (item, value) = case H.lookup item items
                                              of Just v -> v == value
                                                 Nothing -> True

match :: String -> Int -> Int -> Bool
match "cats" v vRead = v > vRead
match "dogs" v vRead = v > vRead
match "pomeranians" v vRead = v < vRead
match "goldfish" v vRead = v < vRead
match _ v v' = v == v'

matchSueRequest' :: Sue -> (String, Int) -> Bool
matchSueRequest' (Sue _ items) (item, value) = case H.lookup item items
                                              of Just v -> match item v value
                                                 Nothing -> True

isMySue :: (Sue -> (String, Int) -> Bool) -> SueItems -> Sue -> Bool
isMySue m mySue otherSue = let l = H.toList mySue
                         in all (m otherSue) l

day16Input = do
  content <- readFile "inputs/day16"
  return $ let (Right res) = parse (parseSue `sepBy` (string "\n")) "BLORK" content in res

day16'' m input = do
  filter (isMySue m mySue) input

day16 = day16'' matchSueRequest
day16' = day16'' matchSueRequest'


-- day17

day17Input :: IO [Int]
day17Input = do
  content <- readFile "inputs/day17"
  return $ reverse (sort (map read (lines content)))

findAllContainers :: [Int] -> [Int] -> Int -> [[Int]]
findAllContainers l currentList n
  | n > 150 = []
  | n == 150 = [currentList]
  | l == [] = []
  | otherwise = let (x:xs) = l
                in findAllContainers xs (x:currentList) (n + x) ++ findAllContainers xs currentList n

day17 :: [Int] -> Int
day17 containers = length (findAllContainers containers [] 0)

day17' :: [Int] -> Int
day17' containers= let res = findAllContainers containers [] 0
                       minL = minimum (map length res)
                       matchingMin = filter (\x -> length x == minL) res
                   in length matchingMin

-- day 18

data Light = LightOn | LightOff deriving (Show, Eq)

type GridLine = V.Vector Light
type Grid = V.Vector GridLine

toLight :: Char -> Light
toLight '#' = LightOn
toLight '.' = LightOff

day18Input :: IO Grid
day18Input = do
  content <- V.fromList <$> map V.fromList <$> (map (map toLight)) <$> (lines <$> readFile "inputs/day18")
  return content

neightbors :: Int -> Int -> [(Int, Int)]
neightbors x y = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard $ (dx, dy) /= (0, 0)
  let (x', y') = (x + dx, y + dy)
  guard $ x' >= 0 && x' < 100
  guard $ y' >= 0 && y' < 100
  return (x', y')

next :: Grid -> Int -> Int -> Light
next items x y = let n = neightbors x y
                     count = length (filter (==LightOn) (map (\(x, y) -> items V.! y V.! x) n))
                     current = items V.! y V.! x
                 in case (current, count)
                    of (LightOff, 3) -> LightOn
                       (LightOn, 2) -> LightOn
                       (LightOn, 3) -> LightOn
                       _ -> LightOff
                     
                                     

step :: Grid -> Grid
step game = V.fromList $ do
  y <- [0..99]
  return $ V.fromList $ do
    x <- [0..99]
    return $ next game x y

day18 :: Int -> Grid -> Int
day18 n game = let gridFinal = head (drop n (iterate step game))
                   s = V.map (V.foldl (\acc x -> if x == LightOn then acc + 1 else acc) 0) gridFinal
               in sum s

mutate18 :: Grid -> Grid
mutate18 game = V.fromList $ do
  y <- [0..99]
  return $ V.fromList $ do
    x <- [0..99]
    return $ case (x, y) of
      (0, 0) -> LightOn
      (99, 99) -> LightOn
      (0, 99) -> LightOn
      (99, 0) -> LightOn
      _ -> game V.! y V.! x

day18' :: Int -> Grid -> Int
day18' n game = let gridFinal = head (drop n (iterate (mutate18.step.mutate18) game))
                    s = V.map (V.foldl (\acc x -> if x == LightOn then acc + 1 else acc) 0) gridFinal
                in sum s

-- day 19

type ActionMolecule = (String, String)

replaceFirst :: String -> [String] -> String -> [String]
replaceFirst from tos s
  | from `isPrefixOf` s = let suffix = drop (length from) s
                              res = map (\prefix -> prefix ++ suffix) tos
                          in res
  | otherwise = []

replace' :: String -> [String] -> String -> [String]
replace' _ _ [] = [[]]
replace' from tos s@(x:xs) = mconcat [map (x:) (replace' from tos xs),
                                      nub (replaceFirst from tos s)
                                     ]

replace :: String -> [String] -> String -> [String]
replace from tos s = drop 1 (replace' from tos s)

replaceAll :: ([(String, [String])], String)-> [String]
replaceAll (actions, s) = nub (mconcat (map (\(from, tos) -> replace from tos s) actions))

parseActions :: String -> ActionMolecule
parseActions s = let [from, _, to] = words s
                 in (from, to)

day19Input :: IO ([(String, [String])], String)
day19Input = do
  content <- lines <$> readFile "inputs/day19"

  let len = length content
      (actions, molecule) = (take (len - 2) content, last content)
      groups = groupBy (\x y -> fst x == fst y) (map parseActions actions)
      okGroups = map (\l -> (fst (head l), (map snd l))) groups

  return (okGroups, molecule)

day19 :: ([(String, [String])], String)-> Int
day19 (actions, s) = length $ replaceAll (actions, s)

invertActions :: ([(String, [String])]) -> ([(String, [String])])
invertActions [] = []
invertActions ((from, tos):xs) = (map (\x -> (x, [from])) tos) ++ invertActions xs

replaceBis' :: String -> [String] -> String -> [String]
replaceBis' _ _ [] = [[]]
replaceBis' from tos s@(x:xs)
  | "Ar" `isPrefixOf` s = [s]
  | otherwise = mconcat [map (x:) (replaceBis' from tos xs),
                         nub (replaceFirst from tos s)
                        ]

replaceBis :: String -> [String] -> String -> [String]
replaceBis from tos s = drop 1 (replaceBis' from tos s)

replaceBisAll :: ([(String, [String])], String)-> [String]
replaceBisAll (actions, s) = nub (mconcat (map (\(from, tos) -> replaceBis from tos s) actions))
                          
reduceToStringFix actions to count s
  | s == to:[] = Just count
  | to `elem` s = Nothing
  | otherwise =           let reduced = replaceBisAll (actions, s)
                              blork = catMaybes (map (reduceToStringFix actions to (count + 1)) reduced)
                          in if null blork
                             then Nothing
                             else Just $ head blork -- We can return the first solution because there is only one solution

reduceString actions count = reduceToStringFix actions 'e' count

actionsExamples = [
  ("e", ["H"]),
  ("e", ["O"]),
  ("H", ["HO"]),
  ("H", ["OH"]),
  ("O", ["HH"])]

day19' (actions, final) = let actions' = invertActions actions
                              actions'' = sortBy srt actions'
                              srt (f, _) (f', _) = compare (length f') (length f)
                          in reduceString actions'' 0 final


-- day 20

day20Input = 34000000 :: Int

reinder max n = zip (map fromIntegral [n, 2*n..max]) (repeat n)

day20hack n mul limit = let len = n `div` mul
                            v :: VU.Vector Int
                            v = VU.accum (+) (VU.replicate (fromIntegral len + 1) 0) all
                            all = mconcat (map (limit . reinder len) [1..len])
                        in VU.findIndex (>=fromIntegral len) v

day20 n = day20hack n 10 id
day20' n = day20hack n 11 (take 50)

-- day 21

data Stat = Stat {hp :: Int, damage :: Int, armor :: Int} deriving (Show)
data Item = Item {cost :: Int, damageBuff :: Int, armorBuff :: Int} deriving (Show, Eq)

-- Data definitions
weapons = [
  Item 8 4 0,
  Item 10 5 0 ,
  Item 25 6 0,
  Item 40 7 0,
  Item 74 8 0
  ]

armors = [
  Item 13 0 1,
  Item 31 0 2,
  Item 53 0 3,
  Item 75 0 4,
  Item 102 0 5,
  Item 0 0 0 -- one null armor
  ]

rings = [
  Item 25 1 0,
  Item 50 2 0,
  Item 100 3 0,
  Item 20 0 1,
  Item 40 0 2,
  Item 80 0 3,
  Item 0 0 0, -- two null rings
  Item 0 0 0
  ]

-- My boss
boss21 = Stat 100 8 2


-- Sum the buyed items and set the character to 100 hp
computeStat :: [Item] -> (Stat, Int)
computeStat items = foldl' foldState ((Stat 100 0 0), 0) items
  where foldState (Stat h d a, c) (Item c' d' a') = (Stat h (d + d') (a + a'), c + c')

-- Compute all equipement combinations
equipement :: [(Stat, Int)]
equipement = do
  w <- weapons
  armor <- armors
  ringa <- rings
  ringb <- filter (/= ringa) rings

  return (computeStat ([w, armor, ringa, ringb]))

-- Sorted.
sortedEquipement = sortWith (\(_, c) -> c) equipement

-- A fight where p0 hits p1
fight :: Stat -> Stat -> Stat
fight p0 p1 = let hit = max 1 (damage p0 - armor p1)
              in p1 {hp = hp p1 - hit}

-- is he dead ?
dead player = hp player <= 0

game boss me
  | dead boss' = True
  | dead me' = False
  | otherwise = game boss' me'
  where boss' = fight me boss
        me' = fight boss' me

day21 = find (\(e, c) -> game boss21 e) sortedEquipement

day21' = maximumWith (filter (\(e, c) -> not (game boss21 e)) equipement) (\(e, c) -> c)

-- day 23

data Register = A | B deriving Show
data Instructions = Hlf Register | Tpl Register | Inc Register | Jump Int | JumpEven Register Int | JumpOne Register Int deriving (Show)

parseOffset = do
  sign <- choice [char '-', char '+']
  value <- decimal

  return $ if sign == '-'
           then negate value
           else value

parseRegister = do
  c <- choice [char 'a', char 'b']

  return $ if c == 'a'
           then A
           else B
  

parseHalf = Hlf <$> (string "hlf " *> parseRegister)
parseTriple = Tpl <$> (string "tpl " *> parseRegister)
parseInt = Inc <$> (string "inc " *> parseRegister)
parseJump = Jump <$> (string "jmp " *> parseOffset)
parseEvenJump = string "jie " *> (JumpEven <$> parseRegister <*> (string ", " *> parseOffset))
parseOneJump = string "jio " *> (JumpOne <$> parseRegister <*> (string ", " *> parseOffset))

parseInstruction = choice [parseHalf, parseTriple, parseInt, try parseJump, try parseEvenJump, parseOneJump]

parseInstructions = parseInstruction `sepBy` (string "\n")

data Computer = Computer (Int, Int) Int deriving (Show)

applyReg :: (Int -> Int) -> (Int, Int) -> Register -> (Int, Int)
applyReg op (ra, rb) A = (op ra, rb)
applyReg op (ra, rb) B = (ra, op rb)

relativeJump test (ra, _) A offset = if test ra
                                        then offset
                                        else 1

relativeJump test (_, rb) B offset = if test rb
                                        then offset
                                        else 1

evalInstruction :: Computer -> Instructions -> Computer
evalInstruction (Computer regs pc) (Hlf register) = Computer (applyReg (`div`2) regs register) (pc + 1)
evalInstruction (Computer regs pc) (Tpl register) = Computer (applyReg (*3) regs register) (pc + 1)
evalInstruction (Computer regs pc) (Inc register) = Computer (applyReg (+1) regs register) (pc + 1)
evalInstruction (Computer regs pc) (Jump offset) = Computer regs (pc + offset)
evalInstruction (Computer regs pc) (JumpEven register offset) = Computer regs (pc + relativeJump even regs register offset)
evalInstruction (Computer regs pc) (JumpOne register offset) = Computer regs (pc + relativeJump (==1) regs register offset)


runProgram :: Computer -> [Instructions] -> Computer
runProgram computer@(Computer _ pc) instructions = if traceShow pc pc < length instructions
                                                   then runProgram (evalInstruction computer (instructions !! pc)) instructions
                                                   else computer

testInstructions = "inc a\njio a, +2\ntpl a\ninc a"

day23Test = let (Right res) = parse parseInstructions "BLORK" testInstructions
            in res

day23Input = do
  content <- readFile "inputs/day23"
  let (Right res) = parse parseInstructions "BLORK" content
  return $ res

day23 input = runProgram (Computer (0, 0) 0) input
day23' input = runProgram (Computer (1, 0) 0) input
