{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Handler.GamePlayers where

import Import
import Control.Concurrent
import Data.Map as Map
import Model.Player
import Model.DominionGamePlay
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (Text,pack,unpack)
import Model.Game
import Model.GameState (board)
import Control.Monad
import Handler.ErrorCode
import Handler.Games
import Network.HTTP.Types (badRequest400)

illegalGamePlay :: Text -> ErrorCode
illegalGamePlay txt = ErrorCode badRequest400 400 txt

illegalPlayer :: ErrorCode
illegalPlayer = ErrorCode badRequest400 401 "Player is not playing in this game"

data PostGamePlayers = PostGamePlayers { choice :: Maybe [Text], decision :: Maybe Bool  } deriving (Generic,Show)
instance Aeson.FromJSON PostMoves

postGamePlayersR :: Int -> Int -> Handler RepJson
postGamePlayersR gameId playerId = do
  player <- readPlayer playerId
  postGamePlayers <- parseJsonBody_
  game <- takeGame gameId
  let result = do
    validatePlayer player
    liftErrorCode illegalGamePlay (liftDominionGame (performMove postGamePlayers) game)
  either (\e -> putGame gameId game >> returnError e)
         (\g -> putGame gameId g >> renderGamePlayer gameId game player)
         result

validatePlayer :: Player -> DominionGame -> Either ErrorCode DominionGame
validatePlayer player dg = if targetPlayer dg == player then Right dg else Left illegalPlayer

performMove :: PostGamePlayers -> DominionGame -> Either Text DominionGame
performMove (PostGamePlayers (Just cs) Nothing) dg = do
    cards <- asCards dg cs
    playerChoice cards dg
performMove (PostGamePlayers Nothing (Just b)) dg = playerDecision b dg
performMove _ _ = Left malformedBody

asCards :: DominionGame -> [Text] -> Either Text [Card]
asCards dg cardNames = forM cardNames cardWithName
    where cardWithName = lookupCard dg

lookupCard :: DominionGame -> Text -> Either Text Card
lookupCard g name = findCard $ Import.map fst (board (dominionGameState g))
    where findCard [] = Left "Unknown card"
          findCard (c:cs) = if cardName c == name then Right c else findCard cs
          
getGamePlayersR :: Int -> Int -> Handler RepJson
getGamePlayersR gameId playerId = do
  player <- readPlayer playerId
  game <- readGame gameId
  renderGamePlayer gameId game player

  
-----

renderGame :: Int -> DominionGame -> Player -> Handler RepJson
renderGame gameId game player = do
	render <- getUrlRender
	jsonToRepJson $ gameToJSON render gameId game player
	
gameToJSON render gid dg player = Aeson.object [
  "status" .= "To be defined" 
	"board" .= show $ board . dominionGameState g, 
	"deck" .= "To be defined", 
	"url" .= render (GamePlayersR gid (pid player))]