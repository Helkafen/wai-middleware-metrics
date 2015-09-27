{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative
import           Control.Concurrent          (forkIO, threadDelay)
import           Network.Wai.Metrics
import           Web.Scotty

import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution
import           System.Remote.Monitoring    (forkServer, serverMetricStore)

readCounters :: WaiMetrics -> IO()
readCounters w = do
  threadDelay 1000000
  v1 <- Counter.read (requestCounter w)
  v2 <- Counter.read (statusCode500Counter w)
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
