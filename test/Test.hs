{-# LANGUAGE OverloadedStrings #-}

module Test where

import           Main
import           Capture
import           Parser
import           Criterion (bench, whnf)
import qualified Data.ByteString.Char8 as C
import           System.IO

quoteAcceptOrdTest :: IO ()
quoteAcceptOrdTest = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap" 
  C.putStrLn <$> runAcceptOrd buf

quotePktOrderTest :: IO ()
quotePktOrderTest = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap"
  C.putStrLn <$> runPktOrd buf

parsePktTest :: IO ()
parsePktTest = do
  let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997"
               :: C.ByteString)
  putStrLn $ show $ parsePkt input

quoteAcceptOrdTime :: String
quoteAcceptOrdTime = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap" 
  (C.putStrLn . show) <$> runAcceptOrd buf

quotePktOrder :: String
quotePktOrder = do
  buf <- readPkts "mdf-kospi200.20110216-0.pcap"
  (C.putStrLn . show) <$> runPktOrd buf

parsePktTime :: String
parsePktTime = do
  let input = ("B6034KR4301F3250500940000679900096000030800095000009400094000023100093000019900092000013400077890009700002340009800001300009900002820010000004150010100000520039700120007000800160009004590011001400170027000709002997"
               :: C.ByteString)
  bench $ whnf $ parsePkt input
