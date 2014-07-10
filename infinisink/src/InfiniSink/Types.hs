{-# LANGUAGE OverloadedStrings #-}

module InfiniSink.Types ( Medium (..)
             , SinkMessage (..)
             , toMedium
             ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?), (.=), FromJSON, object, ToJSON, Value (Object))
import Data.Maybe (catMaybes, Maybe)
import Data.Text (Text)

-- |The 'Medium' through which a message was received
data Medium = Email 
            | Twitter
            | Facebook
            | REST
            | SMS
            | Bookmark
            deriving (Show, Eq, Ord)

-- |'Medium' can be manifested from JSON
instance FromJSON Medium where
    parseJSON (A.String "twitter")  = return Twitter
    parseJSON (A.String "email")    = return Email
    parseJSON (A.String "facebook") = return Facebook
    parseJSON (A.String "rest")     = return REST
    parseJSON (A.String "sms")      = return SMS
    parseJSON (A.String "bookmark") = return Bookmark

    parseJSON _                     = mzero

-- |'Medium' can be serialized to JSON
instance ToJSON Medium where
    toJSON Twitter  = A.String "twitter"
    toJSON Email    = A.String "email"
    toJSON Facebook = A.String "facebook"
    toJSON REST     = A.String "rest"
    toJSON SMS      = A.String "sms"
    toJSON Bookmark = A.String "bookmark"

toMedium :: Text -> Maybe Medium
toMedium "twitter"      = Just Twitter
toMedium "email"        = Just Email
toMedium "facebooke"    = Just Facebook
toMedium "rest"         = Just REST
toMedium "sms"          = Just SMS
toMedium "bookmark"     = Just Bookmark
toMedium _              = Nothing

{-|A message received by InfiniSink. Messages received at the sink have three required fields and one optional:
1. medium: the medium through which the message came
2. message: the actual 'message'
3. payload: an optional payload of other data. Some mediums all for this, e.g. 'email'
4. user: the user that sent the message
5. app: the high-level application using infinisink service.
-}
data SinkMessage = SinkMessage { getMedium    :: Medium
                               , getMessage   :: Text
                               , getPayload   :: Maybe Text
                               , getUser      :: Text
                               , getTimestamp :: Integer
                               , getApp       :: Text
                               } deriving (Show, Eq, Ord)

instance FromJSON SinkMessage where
    parseJSON (Object v) = SinkMessage <$>
                            v .: "medium" <*>
                            v .: "msg" <*>
                            v .:? "payload" <*>
                            v .: "user" <*>
                            v .: "timestamp" <*>
                            v .: "app"
    parseJSON _          = mzero

instance ToJSON SinkMessage where
  toJSON (SinkMessage medium msg payload user ts app) = object $ [ "medium" .= medium
                                                                 , "msg" .= msg
                                                                 , "user" .= user
                                                                 , "timestamp" .= ts
                                                                 , "app" .= app
                                                                 ] ++ catMaybes
                                                                 [ ("payload" .=) . A.String <$> payload ]
