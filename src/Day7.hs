{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Day7 where

-- Day 7
import Text.Parsec
import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)

import Data.Bits
import Data.Maybe (fromJust)
import Data.Function.Memoize (deriveMemoizable, memoFix)

-- Data definitions
newtype Gate = Gate String deriving (Show, Eq, Hashable)
deriveMemoizable ''Gate

data Expr = Lit Int
          | GateExpr Gate
          | And Expr Expr
          | Not Expr
          | Rshift Expr Expr
          | Lshift Expr Expr
          | Or Expr Expr deriving (Show)

deriveMemoizable ''Expr
data Line = Line Expr Gate deriving (Show)

-- utils

decimal = read <$> (many1 (oneOf "0123456789"))

-- Literals
parseLit = Lit <$> decimal
parseGate = Gate <$> many1 (oneOf ['a'..'z'])

parseGateOrLiteral = choice [parseLit, GateExpr <$> parseGate]

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
  gate <- parseGate

  return $ Line expr gate

-- All lines
parseLines = parseLine `sepBy` (string "\n")

day7Input = do
  input <- readFile "inputs/day7"
  return $ let (Right res) = parse parseLines "BLORK" input in res

eval :: H.HashMap Gate Expr -> Expr -> Int
eval commands v = go' v
  where
    go' = memoFix go

    go _ (Lit i) = i
    go go'' (GateExpr gate) = go'' (fromJust (H.lookup gate commands))
    go go'' (And expr0 expr1) = (go'' expr0) .&. (go'' expr1)
    go go'' (Or expr0 expr1) = (go'' expr0)  .|. (go'' expr1)
    go go'' (Rshift expr0 expr1) = (go'' expr0) `shiftR` (go'' expr1)
    go go'' (Lshift expr0 expr1) = (go'' expr0) `shiftL` (go'' expr1)
    go go'' (Not expr) = complement (go'' expr)

gateA = (GateExpr . Gate) "a"

day7 lines = let commands = H.fromList (map (\(Line e name) -> (name, e)) lines)
             in eval commands gateA

day7' lines = let commands = H.fromList (map (\(Line e name) -> (name, e)) lines)
                  retb = eval commands gateA
                  commands' = H.insert (Gate "b") (Lit retb) commands
              in eval commands' gateA
