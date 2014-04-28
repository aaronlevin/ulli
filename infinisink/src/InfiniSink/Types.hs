{-# LANGUAGE OverloadedStrings #-}

module InfiniSink.Types ( Medium (..)
             , SinkMessage (..)
             , toMedium
             ) where

import Control.Applicative ((<*>), (<$>))
import Control.Monad (mzero)
import qualified Data.Aeson as A
import Data.Aeson ((.:), (.:?), (.=), FromJSON, object, ToJSON, Value (Object))
import Data.Maybe (Maybe)
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
1. sink: we scope a sink message by a 'sink'. Most of the time this will be 'ulli', but if we productize this it could be another user.
2. medium: the medium through which the message came
3. message: the actual 'message'
4. payload: an optional payload of other data. Some mediums all for this, e.g. 'email'
5. user: the user that sent the message
-}
data SinkMessage = SinkMessage { getSink :: Text
                               , getMedium :: Medium
                               , getMessage :: Text
                               , getPayload :: Maybe Text
                               , getUser :: Text
                               , getTimestamp :: Integer
                               } deriving (Show, Eq, Ord)

instance FromJSON SinkMessage where
    parseJSON (Object v) = SinkMessage <$>
                            v .: "sink" <*>
                            v .: "medium" <*>
                            v .: "msg" <*>
                            v .:? "payload" <*>
                            v .: "user" <*>
                            v .: "timestamp"
    parseJSON _          = mzero

instance ToJSON SinkMessage where
    toJSON (SinkMessage sink medium msg (Just payload) user ts) = object [ "sink" .= sink
                                                                         , "medium" .= medium
                                                                         , "msg" .= msg
                                                                         , "payload" .= payload
                                                                         , "user" .= user
                                                                         , "timestamp" .= ts
                                                                         ]
    toJSON (SinkMessage sink medium msg Nothing user ts)        = object [ "sink" .= sink
                                                                         , "medium" .= medium
                                                                         , "msg" .= msg
                                                                         , "user" .= user
                                                                         , "timestamp" .= ts
                                                                         ]
