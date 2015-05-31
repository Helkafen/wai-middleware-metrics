{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (replicateM_)
import Data.Int (Int64)

import Test.Tasty
import qualified Test.Tasty.QuickCheck as QC
import qualified Test.QuickCheck.Monadic as QCM
import qualified Data.ByteString as BS

import Web.Scotty (scottyApp, middleware, get, html, raise)

import Network.Wai (Application)
import qualified Network.Wai.Test as WT

import System.Metrics
import qualified System.Metrics.Counter as Counter

import Network.Wai.Metrics

-- Send a GET request to a WAI Application
http_get :: BS.ByteString -> Application -> IO WT.SResponse
http_get path app =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

-- Return the state of Wai Metrics after running n times
-- an action over a fresh scotty server
test_server :: (Application -> IO a) -> Int -> IO WaiMetrics
test_server action times = do
  store <- newStore
  waiMetrics <- addWaiMetrics store
  app <- scottyApp $ do
    middleware (metrics waiMetrics)
    get "/" $ html "Ping"
    get "/error" $ raise "error"
  replicateM_ times (action app)
  return waiMetrics

-- Return the number of requests after running n times
-- an action over a fresh scotty server
read_request_counter :: (Application -> IO a) -> Int -> IO Int64
read_request_counter action times = do
  waiMetrics <- test_server action times
  requests <- Counter.read (requestCounter waiMetrics)
  return requests

-- Return the number of server errors after running n times
-- an action over a fresh scotty server
read_error_counter :: (Application -> IO a) -> Int -> IO Int64
read_error_counter action times = do
  waiMetrics <- test_server action times
  requests <- Counter.read (requestCounter waiMetrics)
  return requests

test_request_counter_scotty :: QC.NonNegative Int -> QC.Property
test_request_counter_scotty (QC.NonNegative n) =  QCM.monadicIO test
  where test = do c <- QCM.run $ read_request_counter (http_get "") n
                  QCM.assert $ (fromIntegral c) == n

test_error_counter_scotty :: QC.NonNegative Int -> QC.Property
test_error_counter_scotty (QC.NonNegative n) =  QCM.monadicIO test
  where test = do c <- QCM.run $ read_error_counter (http_get "/error") n
                  QCM.assert $ (fromIntegral c) == n

tests :: TestTree
tests = testGroup "Metrics tests" [
    QC.testProperty "Request counter must be incremented in middleware" test_request_counter_scotty
  , QC.testProperty "Error counter must be incremented in middleware" test_error_counter_scotty]

main :: IO()
main = defaultMain tests
