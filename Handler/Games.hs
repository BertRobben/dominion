{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Handler.Games where

import Import
import Control.Monad (forM)
import Model.Game
import Model.Player
import GHC.Generics
import Data.Aeson as Aeson
import Handler.ErrorCode
import Handler.Resource
import Handler.Players ()
import Network.HTTP.Types (badRequest400,notFound404)

data PostGames = PostGames { numberOfPlayers :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGames

data PostGamesId = PostGamesId { playerId :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGamesId

unknownGame :: ErrorCode
unknownGame = ErrorCode notFound404 1 "Unknown game"

instance Resource (Game Id) where
  getMapFromApp app = (games app, unknownGame)
  resourceId = gameId
 
illegalJoin :: Text -> ErrorCode
illegalJoin txt = ErrorCode badRequest400 300 txt

getGameR :: Int -> Handler Aeson.Value
getGameR gid = do
  g <- readResource gid
  renderGame g

postGameR :: Int -> Handler Aeson.Value
postGameR gid = do
  postGamesId <- parseJsonBody_
  player <- readResource (playerId postGamesId)
  game <- takeResource gid
  let result = join player game
  either (\t -> putResource game >> returnError (illegalJoin t))
         (\g -> putResource g >> renderGame g)
         result

postGamesR :: Handler Aeson.Value
postGamesR = do
  postGame <- parseJsonBody_
  game <- insertResource (\gid -> newGame (numberOfPlayers postGame) 1 gid)
  renderGame game

getGamesR :: Handler Aeson.Value
getGamesR = do
  allGames <- allResources
  allJsonGames <- forM allGames renderGame
  returnJson allJsonGames
   

initialGames :: [Player] -> [Game Id]
initialGames ps = [g0, g1, g2] where
    g0 = newGame 2 0 0
    g1 = rightGame $ join (ps!!0) (newGame 2 1 1)
    g2 = rightGame $ join (ps!!1) (newGame 2 2 2) >>= join (ps!!2)
    rightGame (Right g) = g
	
---- json rep

renderGame :: Game Id -> Handler Aeson.Value
renderGame g = do
	render <- getUrlRender
	return $ Aeson.object 
          [ "status" .= show g
          , "gid" .= gameId g
          , "url" .= render (GameR (gameId g))
          ]
   
   
   
--
-- POST /games => { "numberOfPlayers": <Int> } -> { "url": <GamesUrl>, "seats" : [Maybe<Player>] }
-- GET /games/#Int 
-- POST /games/#Int => { "player-name": <Text> } -> { "url": <PlayerUrl>, "id" : String }
--    