{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Capture
import           Parser
import           Control.Concurrent.MVar
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map (empty, assocs)
import           Network.Pcap
import           Test.Tasty
import           Test.Tasty.HUnit
import           TextShow (showt)

main :: IO ()
main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Parse one packet" $
    let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997" :: ByteString)
    in (showt $ parsePkt input) @?= "Right (MkDiffTime 2497832520812463.793390821501,Packet {issue_code = \"KR4301F32505\", bids = (Quote {quantity = 134, price = 92},Quote {quantity = 199, price = 93},Quote {quantity = 231, price = 94},Quote {quantity = 94, price = 95},Quote {quantity = 308, price = 96}), asks = (Quote {quantity = 234, price = 97},Quote {quantity = 130, price = 98},Quote {quantity = 282, price = 99},Quote {quantity = 415, price = 100},Quote {quantity = 52, price = 101})})" 
  , testCase "Quotes in order of accept time" $ do
      buf     <- newMVar Map.empty
      handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"
      packets <- dispatchBS handle 10 (enqueueAcceptOrd buf)
      buf <- takeMVar buf
      (showt $ Map.assocs buf) @?= "[(MkDiffTime 0.000000531441,Packet {issue_code = \"KR4301F42959\", bids = (Quote {quantity = 0, price = 2820},Quote {quantity = 9, price = 2825},Quote {quantity = 0, price = 2830},Quote {quantity = 0, price = 2835},Quote {quantity = 9, price = 2840}), asks = (Quote {quantity = 8, price = 3180},Quote {quantity = 0, price = 3185},Quote {quantity = 0, price = 3190},Quote {quantity = 0, price = 3195},Quote {quantity = 8, price = 3200})}),(MkDiffTime 2497941030626089.412558139485,Packet {issue_code = \"KR4201F32705\", bids = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0}), asks = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0})}),(MkDiffTime 3191094069220578.021867675485,Packet {issue_code = \"KR4301F32471\", bids = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0}), asks = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0})})]"
  , testCase "Quotes in order of packet time" $ do
      buf <- newMVar Map.empty
      handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"
      packets <- dispatchBS handle 10 (enqueuePktOrd buf)
      buf <- takeMVar buf
      (showt $ Map.assocs buf) @?= "[(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985359298625536071137902320.086532075209,Packet {issue_code = \"KR4201F32705\", bids = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0}), asks = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0})}),(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985359298625868896525032845.544571648576,Packet {issue_code = \"KR4201F32804\", bids = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0}), asks = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0})}),(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985359298626453461830954269.270630600704,Packet {issue_code = \"KR4301F32471\", bids = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0}), asks = (Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0},Quote {quantity = 0, price = 0})}),(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985375056846359180059805439.904867487744,Packet {issue_code = \"KR4301F32778\", bids = (Quote {quantity = 0, price = 1340},Quote {quantity = 0, price = 1345},Quote {quantity = 0, price = 1350},Quote {quantity = 0, price = 1355},Quote {quantity = 7, price = 1360}), asks = (Quote {quantity = 3, price = 1450},Quote {quantity = 0, price = 1455},Quote {quantity = 0, price = 1460},Quote {quantity = 0, price = 1465},Quote {quantity = 0, price = 1470})}),(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985375240330087550007683403.849853515625,Packet {issue_code = \"KR4301F42629\", bids = (Quote {quantity = 0, price = 505},Quote {quantity = 0, price = 510},Quote {quantity = 0, price = 515},Quote {quantity = 32, price = 520},Quote {quantity = 24, price = 525}), asks = (Quote {quantity = 1, price = 630},Quote {quantity = 0, price = 635},Quote {quantity = 0, price = 640},Quote {quantity = 0, price = 645},Quote {quantity = 0, price = 650})}),(MkDiffTime 22832373657664896871146601377142342376767350580338521212031788788878603985375429248693687978981481.398523069961,Packet {issue_code = \"KR4301F42959\", bids = (Quote {quantity = 0, price = 2820},Quote {quantity = 9, price = 2825},Quote {quantity = 0, price = 2830},Quote {quantity = 0, price = 2835},Quote {quantity = 9, price = 2840}), asks = (Quote {quantity = 8, price = 3180},Quote {quantity = 0, price = 3185},Quote {quantity = 0, price = 3190},Quote {quantity = 0, price = 3195},Quote {quantity = 8, price = 3200})})]"
  ]


-- do {buf <- newMVar Map.empty; handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"; packets <- dispatchBS handle 10 (enqueueAcceptOrd buf); buf <- takeMVar buf; T.putStr $ showt $ Map.assocs buf;}

-- do {buf <- newMVar Map.empty; handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"; packets <- dispatchBS handle 10 (enqueuePktOrd buf); buf <- takeMVar buf; T.putStr $ showt $ Map.assocs buf;}
