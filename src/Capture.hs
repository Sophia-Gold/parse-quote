module Capture where

import           Capture.Types
import           Parser.Types
import           Control.Concurrent.MVar
import           Control.Monad.State.Lazy
import           Data.ByteString (ByteString)
import           Data.Char (intToDigit)
import           Data.IntMap as IntMap
import           Data.Sequence as S
import           Network.Pcap  as P
import           System.IO (FilePath)

readPkts :: FilePath -> IO ()
readPkts path = do
  handle  <- openOffline path
  acceptOrdBuf <- newVar IntMap.empty
  pktOrdBuf <- newVar S.empty
  packets <- dispatch handle (negate 1) (marshallPkts buf)
  stats   <- statistics handle
  let numPkts     = statReceived stats
      packetsRead = (fromIntegral $ toInteger numPkts) - packets 
      msg         = intToDigit packets : " packets processed from dump file."
  case packetsRead of
    0 -> putStrLn msg
    _ -> *> putStrLn msg
         *> putStrLn (intToDigit packetsRead : " packets could not be processed.")

marshallPkts :: QuoteBuffer -> Callback
marshallPkts buf = \pkt -> do
  enqueueAcceptOrd pkt buf
  enqueuePktOrd pkt buf
  -- parsePkt :: (ByteString -> Packet)
  -- let parsedPkt = parsePkt ((curry toBS) pkt)
  -- _ <- enqueueAcceptOrd parsedPkt buf
  -- _ <- enqueuePktOrd parsedPkt buf
  return ()

enqueueAcceptOrd :: Packet -> QuoteBuffer -> QuoteBuffer
enqueueAcceptOrd  pkt buf = do
  oldAcceptBuf <- get $ acceptOrd buf
  newAcceptBuf <- put $ insert 1 pkt oldAcceptBuf
  return QuoteBuffer newAcceptBuf (pktOrd buf)

enqueuePktOrd :: Packet -> QuoteBuffer -> QuoteBuffer
enqueuePktOrd pkt buf = do
  oldPktBuf <- get $ pktOrd buf
  newPktBuf <- put $ pkt <| oldPktBuf
  return QuoteBuffer (acceptOrd buf) newPktBuf
  
dequeueAcceptOrd :: QuoteBuffer -> (QuoteBuffer, Maybe Packet)
dequeueAcceptOrd buf = do
  oldBuf <- get $ acceptOrd buf 
  case minViewWithKey oldBuf of
    Just ((k, v), m) -> (QuoteBuffer (put m) (pktOrd buf), Just v)
    _                -> (buf, Nothing)
    
dequeuePktOrd :: QuoteBuffer -> (QuoteBuffer, Maybe Packet)
dequeuePktOrd buf = do
  oldBuf <- get $ pktOrd buf 
  case viewr oldBuf of
    s :> a -> (QuoteBuffer (acceptOrd buf) (put s), Just a)
    EmptyR -> (buf, Nothing)

runAcceptOrd :: QuoteBuffer -> (IntMap Packet -> IntMap Packet)
runAcceptOrd buf = execState $ acceptOrd buf

runPktOrd :: QuoteBuffer -> (Seq Packet -> Seq Packet)
runPktOrd buf = execState $ pktOrd buf
