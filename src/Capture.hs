module Capture where

import           Capture.Types
import           Parser.Types
import           Control.Concurrent.MVar
import           Control.Monad.State.Lazy
import           Data.ByteString.Char8 (ByteString)
import           Data.Char (intToDigit, digitToInt)
import           Data.IntMap as IntMap
import           Data.Sequence as S
import           Data.Time.Clock (picosecondsToDiffTime)
import           Network.Pcap  as P
import           System.IO (FilePath)

-- readPkts :: FilePath -> IO ()
-- readPkts path buf = do
--   handle  <- openOffline path
--   packets <- dispatchBS handle (negate 1) (marshallPkts buf)
--   stats   <- statistics handle
--   let numPkts     = statReceived stats
--       packetsRead = (fromIntegral $ toInteger numPkts) - packets 
--       msg         = intToDigit packets : " packets processed from dump file."
--   case packetsRead of
--     0 -> putStrLn msg
--     _ -> putStrLn msg *> putStrLn (intToDigit packetsRead : " packets could not be processed.")

-- | should only take one type of buffer
-- marshallPkts :: AcceptTimeBuffer -> PktTimeBuffer -> CallbackBS
-- marshallPkts acceptOrdBuf pktOrdBuf = \pkt -> do
--   _ <- enqueueAcceptOrd (snd pkt) acceptOrdBuf
--   _ <- enqueuePktOrd (snd pkt) pktOrdBuf
--   return ()

-- enqueueAcceptOrd :: Packet -> AcceptTimeBuffer -> IO AcceptTimeBuffer
-- enqueueAcceptOrd  pkt buf =
--   case parsePkt $ snd pkt of
--     Left err -> putStrLn err
--     Right r  -> do
--       oldAcceptBuf <- get buf
--       newAcceptBuf <- put $ insert (fst r) (snd r) buf
--       return newAcceptBuf

-- enqueuePktOrd :: Packet -> PktTimeBuffer -> IO PktTimeBuffer
-- enqueuePktOrd pkt buf =
--   -- | prepend to Seq in parsing order or use IntMap keyed on header timestamp in microseconds?
--   -- let hdr = fst pkt
--   --     t   = (toInteger $ digitToInt $ hdrSeconds hdr)^6
--   --         + (toInteger $ digitToInt $ hdrUSeconds hdr) in
--   case parsePkt $ snd pkt of
--        Left err -> putStrLn err
--        Right r  -> do
--          oldPktBuf <- get buf
--          newPktBuf <- put $ r <| oldPktBuf
--          return newPktBuf
  
-- dequeueAcceptOrd :: AcceptTimeBuffer -> Maybe Packet
-- dequeueAcceptOrd buf = do
--   oldBuf <- get buf 
--   case minViewWithKey oldBuf of
--     Just ((k, v), m) -> put m *> Just v
--     _                -> Nothing
    
-- dequeuePktOrd :: PktTimeBuffer -> Maybe Packet
-- dequeuePktOrd buf = do
--   oldBuf <- get buf 
--   case viewr oldBuf of
--     s :> a -> put s *> Just a
--     EmptyR -> Nothing
