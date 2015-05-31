{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.ByteString as BS
import Control.Monad.IO.Class (liftIO)

import Web.Scotty (scottyApp, middleware, get, html, raise)

import Network.Wai (Application)
import qualified Network.Wai.Test as WT
import qualified System.Metrics.Counter as Counter

import Network.Wai.Metrics (initCounter, metrics)

http_get :: Application -> BS.ByteString -> IO WT.SResponse
http_get app path =
  WT.runSession (WT.srequest (WT.SRequest req "")) app
      where req = WT.setRawPathInfo WT.defaultRequest path

test_counter_null :: TestTree
test_counter_null = testCase "New counter must be null" (
  do (counter, _) <- liftIO initCounter
     value <- Counter.read counter
     value @?= 0)

test_counter_inc :: TestTree
test_counter_inc = testCase "Counter must be incremented" (
  do (counter, _) <- liftIO initCounter
     Counter.inc counter
     value <- Counter.read counter
     value @?= 1)

test_counter_scotty :: TestTree
test_counter_scotty = testCase "Request counter must be incremented in middleware" (
  do (requestCounter,errorCounter) <- liftIO initCounter
     app <- scottyApp $ do
       middleware (metrics requestCounter errorCounter)
       get "/" $ html "Ping"
     _ <- http_get app ""
     _ <- http_get app ""
     requests <- Counter.read requestCounter
     requests @?= 2)


test_error_counter_scotty :: TestTree
test_error_counter_scotty = testCase "Error counter must be incremented in middleware" (
  do (requestCounter,errorCounter) <- liftIO initCounter
     app <- scottyApp $ do
       middleware (metrics requestCounter errorCounter)
       get "/" $ raise "error"
     _ <- http_get app ""
     _ <- http_get app ""
     requests <- Counter.read errorCounter
     requests @?= 2)


tests :: TestTree
tests = testGroup "Unit tests" [
    test_counter_null
  , test_counter_inc
  , test_counter_scotty
  , test_error_counter_scotty]

main :: IO()
main = defaultMain tests
