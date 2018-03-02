{-# LANGUAGE OverloadedStrings #-}

module Bench where

import           Main
import           Capture
import           Parser
import           Control.Monad.State.Lazy (execState)
import           Criterion (bench, whnf)

quoteAcceptOrdTime :: String
quoteAcceptOrdTime = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap" 
  show $ bench "accept time order" (whnf $ execState buf)

quotePktOrder :: String
quotePktOrder = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap"
  show $ bench "packet order" (whnf $ execState buf)

parsePktTime :: String
parsePktTime = do
  let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997"
               :: C.ByteString)
  show $ bench "parser" (whnf $ parsePkt input)
