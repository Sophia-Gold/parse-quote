module Capture.Types where

import           Parser.Types
import           Control.Monad.State.Lazy
import qualified Data.IntMap.Lazy as IntMap
import qualified Data.Sequence as S
  
type AcceptTimeBuffer = State (IntMap.IntMap Packet) ()

type PktTimeBuffer = State (S.Seq Packet) ()

data QuoteBuffer = QuoteBuffer {
    acceptOrd :: AcceptTimeBuffer
  , pktOrd    :: PktTimeBuffer
  }
