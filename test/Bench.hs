{-# LANGUAGE OverloadedStrings #-}

-- | run with: `stack bench --no-nix-pure`

module Main where

import           Capture
import           Parser
import           Parser.Types
import           Control.Concurrent.MVar
import           Criterion.Main
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map (Map, empty, assocs)
import           Data.Text (Text)
import           TextShow (showt)

main :: IO ()
main = defaultMain
  [
    parsePktTime
  , bench "packet order" (whnfIO quoteAcceptOrdTime)
  , bench "accept time order" (whnfIO quotePktOrder)
  , bench "packet order w/ IORef" (whnfIO quoteAcceptOrdTime')
  , bench "accept time order w/ IORef" (whnfIO quotePktOrder')
  -- , perBatchEnv $ env quoteAcceptOrdTime' $ \buf -> bench "packet order" $ whnf (showt <$> Map.assocs buf)
  -- , perBatchEnv $ env quotePktOrder' $ \buf -> bench "accept time order" $ whnf (showt <$> Map.assocs buf)
  ]

parsePktTime :: Benchmark
parsePktTime =
  let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997" :: ByteString)
  in bench "parser" (whnf parsePkt input)
    
quoteAcceptOrdTime :: IO [Text]
quoteAcceptOrdTime = do
  buf <- newMVar Map.empty
  readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueueAcceptOrd buf)
  buf <- takeMVar buf 
  return (showt <$> Map.assocs buf)

quotePktOrder :: IO [Text]
quotePktOrder = do
  buf <- newMVar Map.empty
  readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueuePktOrd buf)
  buf <- takeMVar buf
  return (showt <$> Map.assocs buf)

-- quoteAcceptOrdTime' :: IO [Text]
-- quoteAcceptOrdTime' = do
--   buf <- newIORef Map.empty
--   readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueueAcceptOrd' buf)
--   buf <- readIORef buf 
--   return (showt <$> Map.assocs buf)

-- quotePktOrder' :: IO [Text]
-- quotePktOrder' = do
--   buf <- newIORef Map.empty
--   readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueuePktOrd' buf)
--   buf <- readIORef buf
--   return (showt <$> Map.assocs buf)
    
-- quoteAcceptOrdTime' :: IO (Map.Map AcceptTime (PktTime, Packet))
-- quoteAcceptOrdTime' = do
--   buf <- newMVar Map.empty
--   readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueueAcceptOrd buf)
--   buf <- takeMVar buf 
--   return buf

-- quotePktOrder' :: IO (Map.Map PktTime (AcceptTime, Packet))
-- quotePktOrder' = do
--   buf <- newMVar Map.empty
--   readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueuePktOrd buf)
--   buf <- takeMVar buf
--   return buf
