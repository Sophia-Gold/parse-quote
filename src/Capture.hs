module Capture where

import Parser
import Parser.Types
import Capture.Types
import Control.Concurrent.MVar
import Data.ByteString.Char8 (ByteString)
import Data.Char (intToDigit, digitToInt)
import Data.Data (dataTypeOf, dataTypeName)
import Data.Map.Strict (Map, insert, minViewWithKey)
import Data.Sequence (Seq, ViewR (..), viewr, (<|))
import Data.Time.Clock (picosecondsToDiffTime)
import Network.Pcap
import System.IO (FilePath)

readPkts :: FilePath -> MVar a -> IO ()
readPkts path buf = do
  handle  <- openOffline path
  packets <- dispatchBS handle (negate 1) (marshallPkts buf)
  stats   <- statistics handle
  let numPkts     = statReceived stats
      packetsRead = (fromIntegral $ toInteger numPkts) - packets 
      msg         = intToDigit packets : " packets processed from dump file."
  case packetsRead of
    0 -> putStrLn msg
    _ -> putStrLn msg *> putStrLn (intToDigit packetsRead : " packets could not be processed.")

marshallPkts :: MVar a -> CallbackBS
marshallPkts buf = \hdr pkt -> do
  buf' <- readMVar buf
  case parsePkt pkt of
    Left err -> putStrLn err
    Right (at, p) -> case dataTypeName $ dataTypeOf $ buf' of
      "Data.Sequence.Seq"  -> enqueueAcceptOrd at p buf
      "Data.Map.Map"       -> enqueuePktOrd p buf

enqueueAcceptOrd :: AcceptTime -> Packet -> AcceptTimeBuffer -> IO ()
enqueueAcceptOrd t pkt buf = do
  oldAcceptBuf <- readMVar buf
  newAcceptBuf <- putMVar buf (insert t pkt oldAcceptBuf)
  return ()

enqueuePktOrd :: Packet -> PktTimeBuffer -> IO ()
enqueuePktOrd pkt buf = do
  -- | prepend to Seq in parsing order or use IntMap keyed on header timestamp in microseconds?
  -- let t = (toInteger $ digitToInt $ hdrSeconds hdr)^6
  --       + (toInteger $ digitToInt $ hdrUSeconds hdr) in
  oldPktBuf <- readMVar buf
  newPktBuf <- putMVar buf (pkt <| oldPktBuf)
  return ()
  
dequeueAcceptOrd :: AcceptTimeBuffer -> IO (Maybe Packet)
dequeueAcceptOrd buf = do
  oldBuf <- readMVar buf 
  case minViewWithKey oldBuf of
    Just ((k, v), m) -> putMVar buf m *> pure (Just v)
    _                -> pure Nothing
    
dequeuePktOrd :: PktTimeBuffer -> IO (Maybe Packet)
dequeuePktOrd buf = do
  oldBuf <- readMVar buf 
  case viewr oldBuf of
    s :> a -> putMVar buf s *> pure (Just a)
    EmptyR -> pure Nothing
