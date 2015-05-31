{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Metrics
import Web.Scotty
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import qualified System.Metrics.Counter as Counter

readCounter :: Counter.Counter -> IO()
readCounter c = do
  threadDelay 1000000
  Counter.read c >>= print
  readCounter c

main :: IO()
main = do
  counter <- liftIO initCounter
  _ <- forkIO $ readCounter counter
  scotty 3000 $ do
    middleware (metrics counter)
    get "/" $ html "Ping"
