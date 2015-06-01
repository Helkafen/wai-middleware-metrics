{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Metrics
import Network.Wai.Metrics
import Web.Scotty
import Control.Concurrent (forkIO, threadDelay)
import qualified System.Metrics.Counter as Counter

readCounters :: WaiMetrics -> IO()
readCounters w = do
  threadDelay 1000000
  v1 <- Counter.read (requestCounter w)
  v2 <- Counter.read (serverErrorCounter w)
  print (v1, v2)
  readCounters w

main :: IO()
main = do
  store <- newStore
  waiMetrics <- addWaiMetrics store
  _ <- forkIO $ readCounters waiMetrics
  scotty 3000 $ do
    middleware (metrics waiMetrics)
    get "/" $ html "Ping"
