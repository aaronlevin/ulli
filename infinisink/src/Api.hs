{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (decode)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Text.Encoding (decodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import InfiniSink.Config (infinisinkOpts)
import InfiniSink.Types (SinkMessage (SinkMessage), toMedium)
import Network.HTTP.Types (status400, status409)
import Network.Wai.Middleware.RequestLogger
import Options.Applicative (execParser)
import System.Locale (defaultTimeLocale)
import Web.Scotty (body, middleware, param, post, scotty, status)

-- Process messages as they appear on the queue
processMsgLoop :: Chan SinkMessage -> IO ()
processMsgLoop chan = forever $ do
    msg <- readChan chan
    putStrLn $ show msg

main :: IO()
main = do
    config <- execParser infinisinkOpts
    putStrLn $ show config
    scotty 3000 $ do
        workChannel <- lift newChan
        _ <- lift . forkIO $ processMsgLoop workChannel
        middleware logStdoutDev

        -- |Main endpoint for the sink. We expect a well-formatted JSON document.
        post "/1/sink" $ do
            payload <- body
            case (decode payload) of
                Just message -> liftIO $ writeChan workChannel message
                Nothing -> status status409

        -- |An endpoint that can be used for web-hooks.
        post "/1/:app/:medium/:user" $ do
            appBytes <- param "app"
            mediumBytes <- param "medium"
            userBytes <- param "user"
            payload <- body
            let user = decodeUtf8 userBytes
            let app = decodeUtf8 appBytes
            let message = toStrict $ TLE.decodeUtf8 payload
            currentTime <- liftIO $ read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime

            case (toMedium mediumBytes) of
                Just med -> liftIO $ writeChan workChannel (SinkMessage med message Nothing user currentTime app)
                Nothing -> status status400
