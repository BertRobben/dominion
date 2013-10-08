{-# LANGUAGE DeriveGeneric #-}
module Handler.Players where

import Import
import Control.Monad
import Model.Player
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (pack)
import Handler.Resource
import Handler.ErrorCode
import Network.HTTP.Types (notFound404)

data PostPlayers = PostPlayers { name :: Text } deriving (Generic,Show)
instance Aeson.FromJSON PostPlayers

unknownPlayer :: ErrorCode
unknownPlayer = ErrorCode notFound404 2 "Unknown player"

instance Resource Player where
  getMapFromApp app = (players app, unknownPlayer)
  resourceId = pid
  
postPlayersR :: Handler Aeson.Value
postPlayersR = do
  postPlayer <- parseJsonBody_
  newPlayer <- insertResource (\i -> Player (name postPlayer) i)
  renderPlayer newPlayer

getPlayerR :: Int -> Handler Aeson.Value
getPlayerR pId = do
    p <- readResource pId
    renderPlayer p

getPlayersR :: Handler Aeson.Value
getPlayersR = do
  allPlayers <- allResources
  allJsonPlayers <- forM allPlayers renderPlayer
  returnJson allJsonPlayers

renderPlayer :: Player -> Handler Aeson.Value
renderPlayer p = do
	render <- getUrlRender
	return $ Aeson.object
            [ "playerName" .= playerName p
            , "pid" .= pid p
            , "url" .= render (PlayerR (pid p))
            ]
  
initialPlayers :: [Player]
initialPlayers = playerList ["Bert", "Neo", "Elise", "Thomas"]
	where playerList names = Import.map (\(i,n) -> Player (pack n) i) (zip [0..] names)