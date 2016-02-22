{-# LANGUAGE TemplateHaskell #-}
module Day7 where

-- Day 7
import Text.Parsec
import qualified Data.HashMap.Strict as H
import Data.Bits
import Data.Maybe (fromJust)
import Data.Function.Memoize (deriveMemoizable, memoFix)

decimal = read <$> (many1 (oneOf "0123456789"))

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
