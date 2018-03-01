module Capture.Types where

import           Parser.Types
import           Control.Concurrent.MVar
import           Control.Monad.State.Lazy
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Sequence as S
  
type AcceptTimeBuffer = State (IntMap.IntMap Packet) ()

type PktTimeBuffer = State (S.Seq Packet) ()
  
type AcceptTimeBuffer' = MVar (IntMap.IntMap Packet)

type PktTimeBuffer' = MVar (S.Seq Packet)

-- | seperate buffers for faster pkt order
-- data QuoteBuffer = QuoteBuffer {
--     acceptOrd :: AcceptTimeBuffer
--   , pktOrd    :: PktTimeBuffer
--   }
