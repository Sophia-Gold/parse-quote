module Capture where

import Parser
import Parser.Types
import Capture.Types
import Control.Concurrent.MVar
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map, insert, minViewWithKey)
import Data.Sequence (Seq, ViewR (..), viewr, (<|))
import Data.Time.Clock (picosecondsToDiffTime)
import Network.Pcap
import System.IO (FilePath)

readPkts :: FilePath -> CallbackBS -> IO ()
readPkts path callback = do
  handle  <- openOffline path
  stats   <- statistics handle 
  packets <- dispatchBS handle (- 1) callback
  let numPkts     = fromIntegral $ statReceived stats
      packetsRead = numPkts - packets
      msg         = show numPkts ++ " packets processed from dump file."
  case packetsRead of
    0 -> putStrLn msg
    _ -> putStrLn (msg ++ " " ++ show packetsRead ++ " packets could not be processed.")
    
enqueueAcceptOrd :: AcceptTimeBuffer -> CallbackBS
enqueueAcceptOrd buf = \hdr pkt ->
  case parsePkt pkt of
    Left err -> putStrLn err
    Right (t, p) -> do
      oldAcceptBuf <- takeMVar buf
      newAcceptBuf <- putMVar buf (insert t p oldAcceptBuf)
      return ()
      
enqueuePktOrd :: PktTimeBuffer -> CallbackBS
enqueuePktOrd buf = \hdr pkt ->
  -- | prepend to Seq in parsing order or use IntMap keyed on header timestamp in microseconds?
  -- let t = (toInteger $ digitToInt $ hdrSeconds hdr)^6
  --       + (toInteger $ digitToInt $ hdrUSeconds hdr) in
  case parsePkt pkt of
    Left err -> putStrLn err
    Right (t, p) -> do
      oldPktBuf <- takeMVar buf
      newPktBuf <- putMVar buf (p <| oldPktBuf)
      return ()
  
dequeueAcceptOrd :: AcceptTimeBuffer -> IO (Maybe Packet)
dequeueAcceptOrd buf = do
  oldBuf <- takeMVar buf 
  case minViewWithKey oldBuf of
    Just ((k, v), m) -> putMVar buf m *> pure (Just v)
    _                -> pure Nothing
    
dequeuePktOrd :: PktTimeBuffer -> IO (Maybe Packet)
dequeuePktOrd buf = do
  oldBuf <- takeMVar buf 
  case viewr oldBuf of
    s :> a -> putMVar buf s *> pure (Just a)
    EmptyR -> pure Nothing
