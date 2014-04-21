{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (decode)
import Data.ByteString.Lazy (fromStrict)
import Data.Maybe (Maybe (Nothing, Just))
import Data.Monoid (mconcat)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import InfiniSink.Types (Medium, SinkMessage (SinkMessage), toMedium)
import Network.HTTP.Types (status400, status409)
import Network.Wai.Middleware.RequestLogger
import System.Locale (defaultTimeLocale)
import Web.Scotty (body, middleware, param, post, scotty, status)

-- Process messages as they appear on the queue
processMsgLoop chan = forever $ do
    msg <- readChan chan
    putStrLn $ show msg

main :: IO()
main = do
    scotty 3000 $ do
        workChannel <- lift newChan
        lift . forkIO $ processMsgLoop workChannel
        middleware logStdoutDev

        -- |Main endpoint for the sink. We expect a well-formatted JSON document.
        post "/1/sink" $ do
            payload <- body
            case (decode payload) of
                Just message -> liftIO $ writeChan workChannel message
                Nothing -> status status409

        -- |An endpoint that can be used for web-hooks.
        post "/1/:medium/:user" $ do
            mediumBytes <- param "medium"
            userBytes <- param "user"
            payload <- body
            let user = decodeUtf8 userBytes
            let message = toStrict $ TLE.decodeUtf8 payload
            currentTime <- liftIO $ read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime

            case (toMedium mediumBytes) of
                Just med -> liftIO $ writeChan workChannel (SinkMessage med message Nothing user currentTime)
                Nothing -> status status400
