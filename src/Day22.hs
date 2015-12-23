{-# LANGUAGE TemplateHaskell, LambdaCase #-}
module Day22 where

import Control.Monad.List
import Control.Monad.State

import Data.Foldable

import Control.Lens

trace :: String -> b -> b
trace _ y = y

-- types

data Player = Player {_playerMana :: Int,
                      _playerHP :: Int} deriving (Show)

makeLenses ''Player

data Boss = Boss {_bossHP :: Int,
                  _bossDamage :: !Int} deriving (Show)

makeLenses ''Boss

data Effect = Effect Spell Int deriving (Show)

data Spell = Shield | Poison | Recharge | MagicMissile | Drain deriving (Show, Eq, Read)

data GameState = GameState {
  _boss :: Boss,
  _player :: Player,
  _effects :: [Effect]
  }

makeLenses ''GameState

type BM = (Boss, Player)

-- Datas
-- Everything with a numerical value

shieldArmor :: Int
shieldArmor = 7

applyEffect :: Spell -> BM -> BM
applyEffect Shield = trace "Shield does its effect;" 
applyEffect Poison = trace "Poison deals 3 damage;" $ (_1 . bossHP -~ 3)
applyEffect Recharge = trace "Recharge ups 101 MP;" $ (_2 . playerMana +~ 101)
applyEffect MagicMissile = undefined
applyEffect Drain = undefined

applySpell' :: Spell -> GameState -> GameState
applySpell' MagicMissile = trace "Player casts MagicMissile, dealing 4 damages" $ (boss . bossHP -~ 4)
applySpell' Drain = trace "Player casts Drain" $ ((boss.bossHP -~ 2) . (player.playerHP +~ 2))
applySpell' Shield = trace "Player casts Shield" $ (effects %~ (Effect Shield 6:))
applySpell' Poison = trace "Player casts Poison" $ (effects %~ (Effect Poison 6:))
applySpell' Recharge = trace "Player casts Recharge" $ (effects %~ (Effect Recharge 5:))

applySpell :: Spell -> GameState -> GameState
applySpell spell = (applySpell' spell) .(player.playerMana -~ manaCost spell)
                                                 
manaCost :: Spell -> Int
manaCost MagicMissile = 53
manaCost Drain = 73
manaCost Shield = 113
manaCost Poison = 173
manaCost Recharge = 229

-- Game logic

applyAllEffects :: [Effect] -> BM -> BM
applyAllEffects effects bm = foldl' (\(boss, me) (Effect eff _) -> applyEffect eff (boss, me)) bm effects

decEffect (Effect eff t) = Effect eff (t - 1)

testEffect :: Effect -> Bool
testEffect (Effect _ t) = t > 0

purgeEffects :: [Effect] -> [Effect]
purgeEffects effects = filter testEffect (map decEffect effects)

hasEffect [] _ = False
hasEffect ((Effect effect' _):xs) effect
  | effect == effect' = True
  | otherwise = hasEffect xs effect

preTurn :: GameState -> GameState
preTurn (GameState boss player effects) = let ((boss', player'), effects') = (applyAllEffects effects (boss, player), purgeEffects effects)
                                          in GameState boss' player' effects'

playerArmor :: [Effect] -> Int
playerArmor effects = if effects `hasEffect` Shield then shieldArmor else 0

bossTurn' :: GameState -> GameState
bossTurn' state = let damage = max 1 ((state ^. (boss . bossDamage)) - playerArmor (state ^. effects))
                  in (player.playerHP -~ damage) state

createSpellBook :: [Effect] -> [Spell]
createSpellBook effects = filter (not . (effects`hasEffect`)) [MagicMissile, Drain, Shield, Poison, Recharge]

canCast :: Int -> Spell -> Bool
canCast mana spell = manaCost spell <= mana

playableSpell :: Player -> [Effect] -> [Spell]
playableSpell player effects = filter (canCast (player ^. playerMana)) (createSpellBook effects)

bossTurn :: GameState -> GameState
bossTurn state = bossTurn' (preTurn state)

data Status = Win | Lose | Continue deriving (Show, Eq)

status :: BM -> Status
status bm
  | bm ^. (_1 . bossHP) <= 0 = Win
  | bm ^. (_2 . playerHP) <= 0 = Lose
  | otherwise = Continue

decayNo = id
decayOne = playerHP -~ 1

getBM :: GameState -> BM
getBM state = (state ^. boss, state ^. player)

play :: GameState -> Int -> (Player -> Player) -> [Int]
play game currentCost decay = evalState (runListT (evalStateT (play' currentCost decay) game)) 10000000000

checkEnd game continuation = case (status . getBM) game of
  Lose -> []
  Win -> [[0]]
  Continue -> continuation status

returnAndSetMax val = do
  lift . lift $ put val
  return val

play' :: Int -> (Player -> Player) -> StateT GameState (ListT (State Int)) Int
play' currentCost decay = do
  player %= decay
  modify preTurn

  status . getBM <$> get >>= \case
    Lose -> mzero
    Win -> returnAndSetMax currentCost
    Continue -> do
      game <- get
      let ps = playableSpell (game ^. player) (game ^. effects)

      spell <- lift (ListT (return ps))

      maxMana <- lift . lift $ get
      if currentCost + manaCost spell >= maxMana
        then mzero
        else do
        modify $ applySpell spell

        status . getBM <$> get >>= \case
            Lose -> mzero
            Win -> returnAndSetMax $ manaCost spell + currentCost
            Continue -> do
              modify $ bossTurn

              status . getBM <$> get >>= \case
                Lose -> mzero
                Win -> returnAndSetMax $ manaCost spell + currentCost
                Continue -> play' (manaCost spell + currentCost) decay

{- DEBUG and DISPLAY
playerStatus :: Player -> Int -> String
playerStatus player armor = seq player $ "- Player has " ++ (show (playerHP player)) ++ " hits points, " ++ show armor ++ " armor, " ++ (show (playerMana player)) ++ " mana"

bossStatus :: Boss -> String
bossStatus boss = "- Boss has " ++ (show (bossHP boss)) ++ " hit points"

simulation :: BM -> [Effect] -> [Spell] -> IO ()
simulation _ _ [] = return ()
simulation (boss, player) effects (spell:spells) = do
  putStrLn (seq boss $ seq player (seq effects ""))
  putStrLn ""
  putStrLn "-- Player turn --"
  putStrLn (playerStatus player (playerArmor effects))
  putStrLn (bossStatus boss)
  let
      --playerAlt = player {playerHP = playerHP player - 1}
      playerAlt = player
      ((boss', player'), effects') = preTurn (boss, playerAlt) effects
  putStrLn (seq boss' $ seq player' (seq effects' ""))

  putStrLn $ "You can play " ++ show (playableSpell player' effects')
  putStrLn $ "available spells " ++ show (createSpellBook effects')
  putStrLn $ "You play " ++ show spell
  let ((boss'', player''), effects'') = applySpell ((boss', player'), effects') spell
  print effects''

  putStrLn (seq boss'' $ seq player'' (seq effects'' ""))
  case status (boss'', player'')
    of Lose -> do
         print "LOOSER"
         return ()
       Win -> do print "WINNER"
                 return ()
       Continue -> do
            putStrLn "-- Boss turn --"

            putStrLn (playerStatus player'' (playerArmor effects''))
            putStrLn (bossStatus boss'')
            let ((boss''', player'''), effects''') = preTurn (boss'', player'') effects''

            let (boss'''', player'''') = bossTurn' (boss''', player''') effects'''

            case status (boss'''', player'''')
              of Lose -> do print "LOOSER"
                            return ()
                 Win -> do print "WINNER"
                           return ()
                 Continue -> do
                   simulation (boss'''', player'''') effects''' spells

-}

boss22 = Boss 51 9
me22 = Player 500 50

day22 = let res = play (GameState boss22 me22 []) 0 decayNo
        in minimum res

day22' = let res = play (GameState boss22 me22 []) 0 decayOne
        in minimum res

