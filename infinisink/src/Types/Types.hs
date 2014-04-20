module Types (
    ) where

data Medium = Email 
            | Twitter
            | Facebook
            | REST
            | SMS
            | Bookmark

data SinkMessage = SinkMessage { getMedium :: Medium
                               , getMessage :: Text
                               , getPayload :: Text
                               , getUser :: Text
                               } deriving (Show, Eq, Ord)
