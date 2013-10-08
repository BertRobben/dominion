{-# LANGUAGE OverloadedStrings #-}
module Handler.ErrorCode where

import Import
import Data.Aeson as Aeson
import Data.Text()
import Network.HTTP.Types

data ErrorCode = ErrorCode Status Int Text

returnError :: ErrorCode -> Handler a
returnError (ErrorCode s c msg) = do
    rjson <- returnJson $ Aeson.object [ "errorCode" .= c, "message" .= msg ]
    sendResponseStatus s rjson

malformedBody :: ErrorCode
malformedBody = ErrorCode badRequest400 3 "Malformed body"

liftErrorCode :: (a -> ErrorCode) -> Either a b -> Either ErrorCode b
liftErrorCode err (Left txt) = Left $ err txt
liftErrorCode _ (Right r) = Right r

