{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Concurrent          (threadDelay)
import           Control.Monad               (liftM, replicateM_)
import           Control.Monad.IO.Class      (liftIO)
import           Data.Int                    (Int64)

import qualified Data.ByteString             as BS
import qualified Test.QuickCheck.Monadic     as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import qualified Test.Tasty.QuickCheck       as QC

import           Web.Scotty                  (get, html, middleware, raise,
                                              scottyApp)

import           Network.Wai                 (Application)
import qualified Network.Wai.Test            as WT

import           System.Metrics
import qualified System.Metrics.Counter      as Counter
import qualified System.Metrics.Distribution as Distribution

import           Network.Wai.Metrics

-- Send a GET request to a WAI Application
httpGet :: BS.ByteString -> Application -> IO WT.SResponse
httpGet path =  WT.runSession (WT.srequest (WT.SRequest req ""))
  where req = WT.setRawPathInfo WT.defaultRequest path

between :: Ord a => a -> a -> a -> Bool
between low high x = low <= x && x <= high

-- Return the state of Wai Metrics after running n times
-- an action over a fresh scotty server
testServer :: (Application -> IO a) -> Int -> IO WaiMetrics
testServer action times = do
  store <- newStore
  waiMetrics <- registerWaiMetrics store
  app <- scottyApp $ do
    middleware (metrics waiMetrics)
    get "/"      $ html "Ping"
    get "/error" $ raise "error"
    get "/wait"  $ liftIO (threadDelay 100000) >> html "Ping"
  replicateM_ times (action app)
  return waiMetrics

-- Return the number of requests after running n times
-- an action over a fresh scotty server
readRequestCounter :: (Application -> IO a) -> Int -> IO Int64
readRequestCounter action times = do
  waiMetrics <- testServer action times
  Counter.read (requestCounter waiMetrics)

-- Return the number of server errors after running n times
-- an action over a fresh scotty server
readErrorCounter :: (Application -> IO a) -> Int -> IO Int64
readErrorCounter action times = do
  waiMetrics <- testServer action times
  Counter.read (statusCode500Counter waiMetrics)

-- Return the response time distribution after running n times
-- an action over a fresh scotty server
readResponseTime :: (Application -> IO a) -> Int -> IO Distribution.Stats
readResponseTime action times = do
  waiMetrics <- testServer action times
  Distribution.read (latencyDistribution waiMetrics)

testRequestCounterScotty :: QC.NonNegative Int -> QC.Property
testRequestCounterScotty (QC.NonNegative n) =  QCM.monadicIO test
  where test = do c <- QCM.run $ readRequestCounter (httpGet "") n
                  QCM.assert $ fromIntegral c == n

testErrorCounterScotty :: QC.NonNegative Int -> QC.Property
testErrorCounterScotty (QC.NonNegative n) =  QCM.monadicIO test
  where test = do c <- QCM.run $ readErrorCounter (httpGet "/error") n
                  QCM.assert $ fromIntegral c == n

testResponseTimeScotty :: IO()
testResponseTimeScotty =  do s <- readResponseTime (httpGet "/wait") 3
                             assert $ between 0.1 0.11 (Distribution.mean s)

tests :: TestTree
tests = testGroup "Metrics tests" [
    QC.testProperty "Request counter must be incremented in middleware" testRequestCounterScotty
  , QC.testProperty "Error counter must be incremented in middleware" testErrorCounterScotty
  , testCase "Request time average must be measured in middleware" testResponseTimeScotty]

main :: IO()
main = defaultMain tests
