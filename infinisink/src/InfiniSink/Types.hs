module InfiniSink.Types ( Medium (..)
             , SinkMessage (..)
             ) where

import Data.Maybe (Maybe)
import Data.Text (Text)

data Medium = Email 
            | Twitter
            | Facebook
            | REST
            | SMS
            | Bookmark
            deriving (Show, Eq, Ord)

data SinkMessage = SinkMessage { getMedium :: Medium
                               , getMessage :: Text
                               , getPayload :: Maybe Text
                               , getUser :: Text
                               } deriving (Show, Eq, Ord)
