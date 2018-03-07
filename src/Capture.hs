module Capture where

import Parser
import Parser.Types
import Capture.Types
import Control.Concurrent.MVar
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (insert)
import Data.Time.Clock (picosecondsToDiffTime)
import Network.Pcap
import System.IO (FilePath)

readPkts :: FilePath -> CallbackBS -> IO ()
readPkts path callback = do
  handle  <- openOffline path
  -- stats   <- statistics handle
  packets <- dispatchBS handle (- 1) callback  -- "-1" means loop until end of file
  return ()
  -- let numPkts     = fromIntegral $ statReceived stats
  --     packetsRead = numPkts - packets
  --     msg         = show numPkts ++ " packets processed from dump file."
  -- case packetsRead of
  --   0 -> putStrLn msg
  --   _ -> putStrLn (msg ++ " " ++ show packetsRead ++ " packets could not be processed.")
    
enqueueAcceptOrd :: AcceptTimeBuffer -> CallbackBS
enqueueAcceptOrd buf = \hdr pkt ->
  let pt = PktTime $ picosecondsToDiffTime $ (fromIntegral $ hdrUseconds hdr)^6 in  -- microseconds -> picoseconds
  case parsePkt pkt of
    Left err -> return ()
    Right (at, p) -> modifyMVar_ buf (\m -> pure $ insert at (pt, p) m) 
      
enqueuePktOrd :: PktTimeBuffer -> CallbackBS
enqueuePktOrd buf = \hdr pkt ->
  let pt = PktTime $ picosecondsToDiffTime $ (fromIntegral $ hdrUseconds hdr)^6 in  -- microseconds -> picoseconds
  case parsePkt pkt of
    Left err -> return ()
    Right (at, p) -> modifyMVar_ buf (\m -> pure $ insert pt (at, p) m)
