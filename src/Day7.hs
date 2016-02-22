{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving, QuasiQuotes #-}
module Day7 where

-- Day 7
import Text.Parsec

import qualified Data.HashMap.Strict as H
import Data.Hashable (Hashable)

import Data.Word
import Data.Bits
import Data.Function.Memoize (deriveMemoizable, memoFix, Memoizable, memoize, memoizeFinite)

import Data.String.Here
import Test.Hspec

import Data.Foldable (forM_)

-- Data definitions
newtype Gate = Gate String deriving (Show, Eq, Hashable)

data Expr = Lit Word16
          | GateExpr Gate
          | And Expr Expr
          | Not Expr
          | Rshift Expr Expr
          | Lshift Expr Expr
          | Or Expr Expr deriving (Show, Eq)

-- For memoization
deriveMemoizable ''Gate
instance Memoizable Word16 where memoize = memoizeFinite
deriveMemoizable ''Expr

-- a Type alias
type Program = H.HashMap Gate Expr

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
  return $ (gate, expr)

-- All lines
parseLines = parseLine `sepBy` (string "\n")

day7Content :: IO String
day7Content = readFile "inputs/day7"

day7Parse :: String -> Program
day7Parse input = let (Right res) = parse parseLines "BLORK" input
                  in H.fromList res

day7Input :: IO Program
day7Input = day7Parse <$> day7Content

evalExpr :: Program -> Expr -> Word16
evalExpr commands v = go' v
  where
    go' = memoFix go

    go _ (Lit i) = i
    go go'' (GateExpr gate) = go'' (commands H.! gate)
    go go'' (And expr0 expr1) = (go'' expr0) .&. (go'' expr1)
    go go'' (Or expr0 expr1) = (go'' expr0)  .|. (go'' expr1)
    go go'' (Rshift expr0 expr1) = (go'' expr0) `shiftR` (fromIntegral (go'' expr1))
    go go'' (Lshift expr0 expr1) = (go'' expr0) `shiftL` (fromIntegral (go'' expr1))
    go go'' (Not expr) = complement (go'' expr)

evalGate :: Program -> Gate -> Word16
evalGate commands gate = evalExpr commands (GateExpr gate)

day7 commands = evalGate commands (Gate "a")

day7' commands = let retb = day7 commands
                     commands' = H.insert (Gate "b") (Lit retb) commands
              in evalGate commands' (Gate "a")

testCase = [here|
123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i|]

gateE = GateExpr . Gate

testCaseReaded = H.fromList [
  (Gate "x", Lit 123),
  (Gate "y", Lit 456),
  (Gate "d", And (gateE ("x")) (gateE "y")),
  (Gate "e", Or (gateE "x") (gateE "y")),
  (Gate "f", Lshift (gateE "x") (Lit 2)),
  (Gate "g", Rshift (gateE "y") (Lit 2)),
  (Gate "h", Not (gateE "x")),
  (Gate "i", Not (gateE "y"))
  ]

testResults = [
  ("d", 72),
  ("e", 507),
  ("f", 492),
  ("g", 114),
  ("h", 65412),
  ("i", 65079),
  ("x", 123),
  ("y", 456)
  ]

day7Tests = hspec $ do
  describe "proposed tests" $ do
      let e gate = evalGate (day7Parse testCase) (Gate gate)

      it "parses" $ do
        (day7Parse testCase) `shouldBe` testCaseReaded

      forM_ testResults $ \(s, res) -> do
        it ("works for " ++ s) $ do
          e s `shouldBe` res

  describe "final result" $ do
    it "with first case" $ do
      (day7 <$> day7Input) `shouldReturn` 956
    it "with second case" $ do
      (day7' <$> day7Input) `shouldReturn` 40149
