{-# LANGUAGE OverloadedStrings #-}

module Bench where

import           Capture
import           Parser
import           Control.Concurrent.MVar
import           Criterion (bench, whnf)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map.Strict as Map (empty)
import qualified Data.Sequence as S (empty)
import           Test.Framework (defaultMain)

main :: IO ()
main = defaultMain
  [ quoteAcceptOrdTime
  , quotePktOrderTime
  , parsePktTime
  ]

quoteAcceptOrdTime :: IO String
quoteAcceptOrdTime = do
  acceptOrdBuf <- newMVar Map.empty
  readPkts "mdf-kospi200.20110216-0.pcap" (enqueueAcceptOrd acceptOrdBuf)
  acceptOrdBuf' <- readMVar acceptOrdBuf 
  show $ bench "accept time order" (whnf acceptOrdBuf')

quotePktOrder :: IO String
quotePktOrder = do
  pktOrderBuf <- newMVar S.empty
  dump <- readPkts "mdf-kospi200.20110216-0.pcap" (enqueuePktOrd pktOrderBuf)
  pktOrderBuf' <- readMVar pktOrderBuf
  show $ bench "packet order" (whnf pktOrderBuf')

parsePktTime :: String
parsePktTime =
  let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997"
               :: ByteString)
  in show $ bench "parser" (whnf $ parsePkt input)
