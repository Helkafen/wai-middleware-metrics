{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS

import Web.Scotty (scottyApp, middleware, get, html, raise)

import Network.Wai (Application)
import qualified Network.Wai.Test as WT

import System.Metrics
import qualified System.Metrics.Counter as Counter

import Network.Wai.Metrics --(addWaiMetrics, metrics, WaiMetrics)

http_get :: Application -> BS.ByteString -> IO WT.SResponse
http_get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

test_metrics_null :: TestTree
test_metrics_null = testCase "New metrics must be null" (
  do store <- newStore
     waiMetrics <- addWaiMetrics store
     req <- Counter.read (requestCounter waiMetrics)
     err <- Counter.read (serverErrorCounter waiMetrics)
     (req, err) @?= (0,0))

test_counter_scotty :: TestTree
test_counter_scotty = testCase "Request counter must be incremented in middleware" (
  do store <- newStore
     waiMetrics <- addWaiMetrics store
     app <- scottyApp $ do
       middleware (metrics waiMetrics)
       get "/" $ html "Ping"
     _ <- http_get app ""
     _ <- http_get app ""
     requests <- Counter.read (requestCounter waiMetrics)
     requests @?= 2)


test_error_counter_scotty :: TestTree
test_error_counter_scotty = testCase "Error counter must be incremented in middleware" (
  do store <- newStore
     waiMetrics <- addWaiMetrics store
     app <- scottyApp $ do
       middleware (metrics waiMetrics)
       get "/" $ raise "error"
     _ <- http_get app ""
     _ <- http_get app ""
     requests <- Counter.read (serverErrorCounter waiMetrics)
     requests @?= 2)


tests :: TestTree
tests = testGroup "Unit tests" [
    test_metrics_null
  , test_counter_scotty
  , test_error_counter_scotty]

main :: IO()
main = defaultMain tests
