{-# LANGUAGE DeriveGeneric #-}
module Handler.Games where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Game
import Model.Player
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (pack)
import Handler.Players
import Handler.ErrorCode
import Network.HTTP.Types (badRequest400)

data PostGames = PostGames { numberOfPlayers :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGames

data PostGamesId = PostGamesId { playerId :: Int } deriving (Generic,Show)
instance Aeson.FromJSON PostGamesId

illegalJoin :: Text -> ErrorCode
illegalJoin txt = ErrorCode badRequest400 300 txt

getGameR :: Int -> Handler RepJson
getGameR gameId = do
  g <- readGame gameId
  renderGame (gameId,g)

postGameR :: Int -> Handler RepJson
postGameR gameId = do
  postGamesId <- parseJsonBody_
  player <- readPlayer (playerId postGamesId)
  game <- takeGame gameId
  let result = join player game
  either (\t -> putGame gameId game >> returnError (illegalJoin t))
         (\g -> putGame gameId g >> renderGame (gameId,g))
         result

readGameMVar :: Int -> Handler (MVar Game)
readGameMVar gameId = do
  yesod <- getYesod
  allGames <- liftIO $ readMVar $ games yesod
  case Map.lookup gameId allGames of
    Nothing -> returnError unknownGame
    Just mvg -> return mvg

readGame :: Int -> Handler Game
readGame gameId = do
  mvg <- readGameMVar gameId
  liftIO $ readMVar mvg

takeGame :: Int -> Handler Game
takeGame gameId = do
  mvg <- readGameMVar gameId
  liftIO $ takeMVar mvg

putGame :: Int -> Game -> Handler Game
putGame gameId g = do
  mvg <- readGameMVar gameId
  liftIO $ putMVar mvg g


postGamesR :: Handler RepJson
postGamesR = do
  postGame <- parseJsonBody_
  yesod <- getYesod
  allGames <- liftIO $ takeMVar $ games yesod
  let g = newGame (numberOfPlayers postGame) 1
  mvg <- liftIO $ newMVar g
  let newGameId = Map.size allGames 
      allGames' = Map.insert newGameId mvg allGames 
  liftIO $ putMVar (games yesod) allGames'
  renderGame (newGameId, g)

getGamesR :: Handler RepJson
getGamesR = do
  yesod <- getYesod
  render <- getUrlRender
  allGames <- liftIO $ readMVar $ games yesod
  jsonToRepJson $ Import.map (gameToJSON render) (Map.toList allGames)
   

initialGames :: Map.Map Int Player -> IO (Map.Map Int (MVar Game))
initialGames players = do
    g0 <- newMVar (newGame 2 0)
    g1 <- newMVar (rightGame $ join p0 (newGame 2 1))
    g2 <- newMVar (rightGame $ join p1 (newGame 2 2) >>= join p2)
    return $ Map.fromList [(0,g0),(1,g1),(2,g2)]
    where
	p0 = players Map.! 0
	p1 = players Map.! 1
	p2 = players Map.! 2
	rightGame (Right g) = g
	
---- json rep

renderGame :: (Int,Game) -> Handler RepJson
renderGame g = do
	render <- getUrlRender
	jsonToRepJson $ gameToJSON render g
	
gameToJSON render (gid,g) = Aeson.object [
	"status" .= show g, 
	"gid" .= gid, 
	"url" .= render (GameR gid)]
   
   
   
--
-- POST /games => { "numberOfPlayers": <Int> } -> { "url": <GamesUrl>, "seats" : [Maybe<Player>] }
-- GET /games/#Int 
-- POST /games/#Int => { "player-name": <Text> } -> { "url": <PlayerUrl>, "id" : String }
--    