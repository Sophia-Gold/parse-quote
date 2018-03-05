module Capture where

import Parser
import Parser.Types
import Capture.Types
import Control.Concurrent.MVar
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (Map, insert, minViewWithKey)
import Data.Time.Clock (picosecondsToDiffTime)
import Network.Pcap
import System.IO (FilePath)

readPkts :: FilePath -> CallbackBS -> IO ()
readPkts path callback = do
  handle  <- openOffline path
  stats   <- statistics handle 
  packets <- dispatchBS handle (- 1) callback
  return ()
  -- let numPkts     = fromIntegral $ statReceived stats
  --     packetsRead = numPkts - packets
  --     msg         = show numPkts ++ " packets processed from dump file."
  -- case packetsRead of
  --   0 -> putStrLn msg
  --   _ -> putStrLn (msg ++ " " ++ show packetsRead ++ " packets could not be processed.")
    
enqueueAcceptOrd :: AcceptTimeBuffer -> CallbackBS
enqueueAcceptOrd buf = \hdr pkt ->
  case parsePkt pkt of
    Left err -> return ()
    Right (t, p) -> do
      oldAcceptBuf <- takeMVar buf
      newAcceptBuf <- putMVar buf (insert t p oldAcceptBuf)
      return ()
      
enqueuePktOrd :: PktTimeBuffer -> CallbackBS
enqueuePktOrd buf = \hdr pkt ->
  let pt = picosecondsToDiffTime $ (fromIntegral $ hdrSeconds hdr)^12     -- seconds -> picoseconds
                                 + (fromIntegral $ hdrUseconds hdr)^6 in  -- microseconds -> picoseconds
  case parsePkt pkt of
    Left err -> return ()
    Right (t, p) -> do
      oldPktBuf <- takeMVar buf
      newPktBuf <- putMVar buf (insert pt p oldPktBuf)
      return ()
  
-- dequeueAcceptOrd :: AcceptTimeBuffer -> IO (Maybe Packet)
-- dequeueAcceptOrd buf = do
--   oldBuf <- takeMVar buf 
--   case minViewWithKey oldBuf of
--     Just ((k, v), m) -> putMVar buf m *> pure (Just v)
--     _                -> pure Nothing
    
-- dequeuePktOrd :: PktTimeBuffer -> IO (Maybe Packet)
-- dequeuePktOrd buf = do
--   oldBuf <- takeMVar buf 
--   case viewr oldBuf of
--     s :> a -> putMVar buf s *> pure (Just a)
--     EmptyR -> pure Nothing
