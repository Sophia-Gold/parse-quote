{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Capture
import           Capture.Types
import           Parser
import           Parser.Types
import qualified Data.ByteString.Char8 as C
import           Data.List (sortOn)
import           Date.Time.Clock
import           System.Environment

-- n is length of stream in seconds
pcapStream :: Integer -> [Packet]
pcapStream s = go (getCurrentTime >>= flip $ addUTCTime $ secondToDiffTime s) getCurrentTime [] where
  go s result = if getCurrentTime >>= (\t -> s >= t)
                  then result
                  else go s (nextPacket : result) -- | placeholder for `nextPacket` function

-- default to 30s
pcapStream' :: [Packet]
pcapStream' = pcapStream 30

acceptOrd :: [Packet] -> [Packet]
acceptOrd p = sortOn (acceptTime p) p

pktOrd :: [Packet] -> [Packet]
pktOrd p = sortOn (pktTime p) p

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "-r" -> C.putStrLn <$> acceptOrd pcapStream'
    _    -> C.putStrLn <$> pktOrd pcapStream'
