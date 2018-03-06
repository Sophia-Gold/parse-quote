{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Capture
import           Parser
import           Control.Concurrent.MVar
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map (empty, assocs)
import qualified Data.Text as T
import           Network.Pcap
import           Test.Tasty
import           Test.Tasty.HUnit
import           TextShow (showt, printT)

main :: IO ()
main = defaultMain tests
  
tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Parse one packet" $
    let input = "B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997"
    in (showt $ parsePkt input) @?= "Right ( Accept-Time: 2497832520812463.793390821501s, Issue-Code: \"KR4301F32505\", Bids:( 134@92, 199@93, 231@94, 94@95, 308@96), Asks:( 234@97, 130@98, 282@99, 415@100, 52@101))"
  , testCase "Quotes in order of accept time" $ do
      buf     <- newMVar Map.empty
      handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"
      packets <- dispatchBS handle 10 (enqueueAcceptOrd buf)
      buf <- takeMVar buf
      (showt $ (\(at,(pt,p)) -> (pt,at,p)) <$> Map.assocs buf) @?= "[(Packet-Time: 16130623157687978981481.398523069961s, Accept-Time: 0.000000531441s, Issue-Code: \"KR4301F42959\", Bids:( 0@2820, 9@2825, 0@2830, 0@2835, 9@2840), Asks:( 8@3180, 0@3185, 0@3190, 0@3195, 8@3200)),(Packet-Time: 71137902320.086532075209s, Accept-Time: 2497941030626089.412558139485s, Issue-Code: \"KR4201F32705\", Bids:( 0@0, 0@0, 0@0, 0@0, 0@0), Asks:( 0@0, 0@0, 0@0, 0@0, 0@0)),(Packet-Time: 917461830954269.270630600704s, Accept-Time: 3191094069220578.021867675485s, Issue-Code: \"KR4301F32471\", Bids:( 0@0, 0@0, 0@0, 0@0, 0@0), Asks:( 0@0, 0@0, 0@0, 0@0, 0@0))]"
  , testCase "Quotes in order of packet time" $ do
      buf <- newMVar Map.empty
      handle  <- openOffline "test/mdf-kospi200.20110216-0.pcap"
      packets <- dispatchBS handle 10 (enqueuePktOrd buf)
      buf <- takeMVar buf
      (showt $ (\(pt,(at,p)) -> (pt,at,p)) <$> Map.assocs buf) @?= "[(Packet-Time: 71137902320.086532075209s, Accept-Time: 2497941030626089.412558139485s, Issue-Code: \"KR4201F32705\", Bids:( 0@0, 0@0, 0@0, 0@0, 0@0), Asks:( 0@0, 0@0, 0@0, 0@0, 0@0)),(Packet-Time: 332896525032845.544571648576s, Accept-Time: 3191094069220578.021867675485s, Issue-Code: \"KR4201F32804\", Bids:( 0@0, 0@0, 0@0, 0@0, 0@0), Asks:( 0@0, 0@0, 0@0, 0@0, 0@0)),(Packet-Time: 917461830954269.270630600704s, Accept-Time: 3191094069220578.021867675485s, Issue-Code: \"KR4301F32471\", Bids:( 0@0, 0@0, 0@0, 0@0, 0@0), Asks:( 0@0, 0@0, 0@0, 0@0, 0@0)),(Packet-Time: 15758220823180059805439.904867487744s, Accept-Time: 0.000000531441s, Issue-Code: \"KR4301F32778\", Bids:( 0@1340, 0@1345, 0@1350, 0@1355, 7@1360), Asks:( 3@1450, 0@1455, 0@1460, 0@1465, 0@1470)),(Packet-Time: 15941704551550007683403.849853515625s, Accept-Time: 0.000000531441s, Issue-Code: \"KR4301F42629\", Bids:( 0@505, 0@510, 0@515, 32@520, 24@525), Asks:( 1@630, 0@635, 0@640, 0@645, 0@650)),(Packet-Time: 16130623157687978981481.398523069961s, Accept-Time: 0.000000531441s, Issue-Code: \"KR4301F42959\", Bids:( 0@2820, 9@2825, 0@2830, 0@2835, 9@2840), Asks:( 8@3180, 0@3185, 0@3190, 0@3195, 8@3200))]"
  ]
