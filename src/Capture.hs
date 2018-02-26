module Capture where

import           Capture.Types
import qualified Data.ByteString as BS
import qualified Data.Vector     as V
import qualified Network.Pcap    as P
import           System.IO

nextPacket :: FilePath -> IO ()
nextPacket path = do
  handle  <- P.openOffline path
  numPkts <- toInteger $ P.statsReceived $ P.statistics handle
  packets <- P.dispatch handle -1 marshallPkts
    let packetsRead = numPkts - packets
  case packetsRead of
    0 -> putStrLn (packets ++ "packets processed from dump file.")
         *> ()
           
    _ -> putStrLn (packets ++ "packets processed from dump file.")
         *> putStrLn (packetsRead ++ "could not be processed.")
         *> ()

-- | should be of type `P.Callback` which returns `IO ()` 
marshallPkts :: P.Callback -> Vector (ByteString)
marshallPkts pkt = do
  bs <- snd $ curry . P.toBS pkt
  fmap (\bs -> go V.empty bs) bs where
    go v BS.empty = return liftIO v
    go v bs = V.cons bs v
