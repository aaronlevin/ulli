{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class (lift)
import InfiniSink.Types (SinkMessage)
import Data.ByteString (ByteString)
import Data.Monoid (mconcat)
import Network.Wai.Middleware.RequestLogger
import Web.Scotty (body, middleware, post, scotty)

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
        post "/1/sink" $ do
            payload <- body
            -- process payload then put on channel
            liftIO $ writeChan workChannel payload
