{-# LANGUAGE TypeSynonymInstances,FlexibleInstances,OverloadedStrings #-}
module Model.Game (

  Game,
  join,
  newGame,
  dominionGame,
  asGame,
  liftDominionGame,
  gameId
    
) where

import Prelude
import Model.Player
import Model.DominionGamePlay
import Model.Card
import Model.GameState
import Data.Text (Text)

newtype Game a = Game (a, Either (Int, Int, [Player]) DominionGame)

instance Show (Game a) where
  show (Game (_,g)) = either (\(n,_,ps) -> "Waiting for " ++ show n ++ " players to join."
                                        ++ "\n"
                                        ++ "Current players: " ++ show ps)
                           (const "Game in progress.")
                           g 
  
newGame :: Int -> Int -> a -> Game a
newGame n seed a = Game (a, Left (n, seed, []))

gameId :: Game a -> a
gameId (Game (gid,_)) = gid

join :: Player -> Game a -> Either Text (Game a)
join p (Game (gid, Left startUp)) = Right $ Game (gid, addPlayer p startUp) 
join _ _ = Left "Game is in progress. It's no longer possible to join."

addPlayer :: Player -> (Int, Int, [Player]) -> Either (Int, Int, [Player]) DominionGame 
addPlayer p (n,s,ps) = if n == 1 then Right (simpleGame s (p:ps)) else Left (n-1, s, p:ps)
                                          
simpleGame :: Int -> [Player] -> DominionGame
simpleGame seed ps = newDominionGame simpleState [province] where
  startDeck = [estate, estate, estate, copper, copper, copper, copper, copper, copper, copper]
  simpleBoard = [(copper, 50), (silver, 30), (gold, 20), 
                 (village, 10), (chapel, 10), 
                 (estate, 8), (duchy, 8), (province, 8)]
  simpleState = initialState seed (map (\p -> (p, startDeck)) ps) simpleBoard

dominionGame :: Game a -> Maybe DominionGame
dominionGame (Game (_,Right dg)) = Just dg
dominionGame _ = Nothing

asGame :: a -> DominionGame -> Game a
asGame a dg = Game (a, Right dg)

liftDominionGame :: (DominionGame -> Either Text DominionGame) -> Game a -> Either Text (Game a)
liftDominionGame f (Game (gid, Right dg)) = fmap withDominionGame (f dg) where
    withDominionGame d = Game (gid, Right d)
liftDominionGame _ _ = Left "Illegal state -- game is not started."