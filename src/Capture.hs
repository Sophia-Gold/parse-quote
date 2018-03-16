module Capture where

import Parser
import Parser.Types
import Capture.Types
import Control.Concurrent.MVar
import Data.ByteString.Char8 (ByteString)
import Data.Map.Strict (insert)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), picosecondsToDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
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
  let pt = PktTime $ posixSecondsToUTCTime $ realToFrac $
                       (fromIntegral $ hdrSeconds hdr)
                     + (fromIntegral $ hdrUseconds hdr)*10^^(-6)  -- microseconds -> seconds
                     + 64800 in  -- UTC+9in 
  case parsePkt pkt of
    Left err -> return ()
    Right (at, p) -> modifyMVar_ buf (\m -> pure $ insert at (pt, p) m) 
      
enqueuePktOrd :: PktTimeBuffer -> CallbackBS
enqueuePktOrd buf = \hdr pkt ->
  let pt = PktTime $ posixSecondsToUTCTime $ realToFrac $
                       (fromIntegral $ hdrSeconds hdr)
                     + (fromIntegral $ hdrUseconds hdr)*10^^(-6)  -- microseconds -> seconds 
                     + 64800 in  -- UTC+9
  case parsePkt pkt of
    Left err -> return ()
    Right (at, p) -> modifyMVar_ buf (\m -> pure $ insert pt (at, p) m)
