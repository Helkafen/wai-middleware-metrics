{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Metrics
import Web.Scotty
import Control.Applicative
import Control.Concurrent (forkIO, threadDelay)

import System.Remote.Monitoring (serverMetricStore, forkServer)
import qualified System.Metrics.Counter as Counter
import qualified System.Metrics.Distribution as Distribution

readCounters :: WaiMetrics -> IO()
readCounters w = do
  threadDelay 1000000
  v1 <- Counter.read (requestCounter w)
  v2 <- Counter.read (serverErrorCounter w)
  v3 <- Distribution.mean <$> Distribution.read (latencyDistribution w)
  print (v1, v2, v3)
  readCounters w

main :: IO()
main = do
  store <- serverMetricStore <$> forkServer "localhost" 8000
  waiMetrics <- registerWaiMetrics store
  _ <- forkIO $ readCounters waiMetrics
  scotty 3000 $ do
    middleware (metrics waiMetrics)
    get "/" $ html "Ping"
