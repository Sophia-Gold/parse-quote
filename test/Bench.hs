{-# LANGUAGE OverloadedStrings #-}

-- | run with: `stack bench --no-nix-pure`

module Main where

import           Capture
import           Parser
import           Parser.Types
import           Control.Concurrent.Async.Timer
import           Control.Concurrent.MVar
import           Criterion.Main
import           Data.ByteString.Char8 (ByteString)
import           Data.IORef
import qualified Data.Map.Strict as Map
import           Data.Text (Text)
import           Network.Pcap
import           TextShow (showt, printT)

main :: IO ()
main = defaultMain
  [
    parsePktTime
  , bench "packet order" (whnfIO quoteAcceptOrdTime)
  , bench "accept time order" (whnfIO quotePktOrder)
  , bench "incremental packet order" (whnfIO quoteAcceptOrdTime'')
  , bench "incremental accept time order" (whnfIO quotePktOrder'')
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

quoteAcceptOrdTime'' :: IO ()
quoteAcceptOrdTime'' = do
  buf <- newMVar Map.empty
  readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueueAcceptOrd buf)
  let conf = timerConfSetInitDelay 4000 $ timerConfSetInterval 4000 $ defaultTimerConf -- 4s delay
  withAsyncTimer conf $ \ timer -> do
    oldBuf <- takeMVar buf
    let m' = Map.assocs oldBuf
        m = splitAt (length m' `div ` 4 * 3) m'
    newBuf <- putMVar buf (Map.fromList $ snd m)
    return (showt <$> (fst m)) 
    timerWait timer
      
quotePktOrder'' :: IO ()
quotePktOrder'' = do
  buf <- newMVar Map.empty
  readPkts "test/mdf-kospi200.20110216-0.pcap" (enqueuePktOrd buf)
  let conf = timerConfSetInitDelay 4000 $ timerConfSetInterval 4000 $ defaultTimerConf -- 4s delay
  withAsyncTimer conf $ \ timer -> do
    oldBuf <- takeMVar buf
    let m' = Map.assocs oldBuf
        m = splitAt (length m' `div ` 4 * 3) m'
    newBuf <- putMVar buf (Map.fromList $ snd m)
    return (showt <$> (fst m))
    timerWait timer
