{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Handler.GamePlayers where

import Import
import Model.Player
import Model.DominionGamePlay
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (pack)
import Model.Game
import Model.GameState (board)
import Control.Monad
import Handler.ErrorCode
import Handler.Resource
import Handler.Players ()
import Handler.Games ()
import Network.HTTP.Types (badRequest400)

illegalGamePlay :: Text -> ErrorCode
illegalGamePlay txt = ErrorCode badRequest400 400 txt

illegalPlayer :: ErrorCode
illegalPlayer = ErrorCode badRequest400 401 "Player is not playing in this game"

unknownCard :: ErrorCode
unknownCard = ErrorCode badRequest400 402 "Can't use a card that is not a part of this game"

gameNotStarted :: ErrorCode
gameNotStarted = ErrorCode badRequest400 403 "Game has not yet started."

data PostGamePlayers = PostGamePlayers { choice :: Maybe [Text], decision :: Maybe Bool  } deriving (Generic,Show)
instance Aeson.FromJSON PostGamePlayers

postGamePlayersR :: Id -> Id -> Handler Aeson.Value
postGamePlayersR gid pid = do
  player <- readResource pid
  postGamePlayers <- parseJsonBody_
  game <- takeResource gid
  let result = asDominionGame game >>= validatePlayer player >>= performMove postGamePlayers
  either (\e -> putResource game >> returnError e)
         (\dg -> putResource (asGame gid dg) >>= (\g -> renderGamePlayer g player))
         result

asDominionGame :: Game Id -> Either ErrorCode DominionGame
asDominionGame g = maybe (Left gameNotStarted) Right (dominionGame g)

validatePlayer :: Player -> DominionGame -> Either ErrorCode DominionGame
validatePlayer player dg = if targetPlayer dg == player then Right dg else Left illegalPlayer

performMove :: PostGamePlayers -> DominionGame -> Either ErrorCode DominionGame
performMove (PostGamePlayers (Just cs) Nothing) dg = do
    cards <- asCards dg cs
    liftErrorCode illegalGamePlay (playerChoice cards dg)
performMove (PostGamePlayers Nothing (Just b)) dg = liftErrorCode illegalGamePlay (playerDecision b dg)
performMove _ _ = Left malformedBody

asCards :: DominionGame -> [Text] -> Either ErrorCode [Card]
asCards dg cardNames = forM cardNames cardWithName
    where cardWithName = lookupCard dg

lookupCard :: DominionGame -> Text -> Either ErrorCode Card
lookupCard g name = findCard $ Import.map fst (board (dominionGameState g))
    where findCard [] = Left unknownCard
          findCard (c:cs) = if cardName c == name then Right c else findCard cs
          
getGamePlayersR :: Id -> Id -> Handler Aeson.Value
getGamePlayersR gid pid = do
  player <- readResource pid
  game <- readResource gid
  renderGamePlayer game player

  
-----

renderGamePlayer :: Game Id -> Player -> Handler Aeson.Value
renderGamePlayer game player = do
	render <- getUrlRender
	return $ Aeson.object 
            [ "status" .= pack "To be defined"
            , "board" .= pack "To be defined"
            ,	"deck" .= pack "To be defined"
            ,	"url" .= render (GamePlayersR (gameId game) (pid player))]