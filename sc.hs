{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Metrics
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import qualified System.Metrics.Counter as Counter

readCounter :: Counter.Counter -> Counter.Counter -> IO()
readCounter c e = do
  threadDelay 1000000
  v1 <- Counter.read c
  v2 <- Counter.read e
  print (v1, v2)
  readCounter c e

main :: IO()
main = do
  (requestCounter, errorCounter) <- liftIO initCounter
  _ <- forkIO $ readCounter requestCounter errorCounter
  scotty 3000 $ do
    middleware (metrics requestCounter errorCounter)
    get "/" $ html "Ping"
