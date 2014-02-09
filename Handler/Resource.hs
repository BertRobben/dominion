module Handler.Resource where

import Import
import Control.Concurrent
import Control.Monad (forM)
import Data.Map as Map
import Handler.ErrorCode
import System.Random

class Resource a where
  getMapFromApp :: App -> (ResourceMap a, ErrorCode)
  resourceId :: a -> Id

newResourceMap :: (Resource a) => [a] -> IO (ResourceMap a)
newResourceMap aa = do
  resources <- forM aa (\a -> newMVar a >>= (\mv -> return (resourceId a, mv)))
  newMVar (Map.fromList resources)

allResources :: (Resource a) => Handler [a]
allResources = do
  yesod <- getYesod
  let (resourceMap, _) = getMapFromApp yesod
  resources <- liftIO $ readMVar resourceMap
  forM (Map.elems resources) (liftIO . readMVar)
  
readResourceMVar :: (Resource a) => Id -> Handler (MVar a)
readResourceMVar rid = do
  yesod <- getYesod
  let (resourceMap, unknownResource) = getMapFromApp yesod 
  resources <- liftIO $ readMVar resourceMap
  case Map.lookup rid resources of
    Nothing -> returnError unknownResource
    Just mvg -> return mvg

readResource :: (Resource a) => Id -> Handler a
readResource rid = do
  mvg <- readResourceMVar rid
  liftIO $ readMVar mvg

takeResource :: (Resource a) => Id -> Handler a
takeResource rid = do
  mvg <- readResourceMVar rid
  liftIO $ takeMVar mvg

putResource :: (Resource a) => a -> Handler a
putResource a = do
  yesod <- getYesod
  let (resourceMap, _) = getMapFromApp yesod 
  resources <- liftIO $ readMVar resourceMap
  case Map.lookup (resourceId a) resources of
    Nothing -> addResource a
    Just mvg -> liftIO $ putMVar mvg a >> return a
    
addResource :: (Resource a) => a -> Handler a
addResource a = do
  yesod <- getYesod
  let (resourceMap, _) = getMapFromApp yesod 
  liftIO $ do 
    resources <- takeMVar resourceMap
    newResource <- newMVar a
    putMVar resourceMap (Map.insert (resourceId a) newResource resources) 
    return a

insertResource :: (Resource a) => (Id -> a) -> Handler a
insertResource f = do
  newId <- liftIO $ do
    stdGen <- getStdGen
    return $ fst (next stdGen)
  putResource (f newId)
