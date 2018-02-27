module Capture where

import           Capture.Types
import qualified Data.ByteString as BS
import           Data.Char (intToDigit)
import           Network.Pcap    as P
import           System.IO (FilePath)

readPkts :: FilePath -> IO ()
readPkts path = do
  handle  <- openOffline path
  packets <- dispatch handle (negate 1) marshallPkts
  stats   <- statistics handle
  let numPkts     = statReceived stats
      packetsRead = (fromIntegral $ toInteger numPkts) - packets 
      msg         = intToDigit packets : " packets processed from dump file."
  case packetsRead of
    0 -> putStrLn msg
           
    _ -> putStrLn msg
         *> putStrLn (intToDigit packetsRead : " packets could not be processed.")

-- | should be of type `P.Callback` which returns `IO ()` rather than IO (Vector (ByteString)) 
marshallPkts :: Callback
marshallPkts = \pkt -> do
  let bs = (curry toBS) pkt
  return (putStrLn " ")
  -- fmap (\bs -> go V.empty bs) (snd bs) where
  --   go v empty = v
  --   go v bs = V.cons bs v
