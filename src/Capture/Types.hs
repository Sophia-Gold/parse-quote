module Capture.Types where

import Parser.Types
import Control.Concurrent.MVar (MVar)
-- import Control.Monad.State.Lazy (State)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
  
-- type AcceptTimeBuffer = State (Map AcceptTime Packet) ()

-- type PktTimeBuffer = State (Seq Packet) ()
  
type AcceptTimeBuffer = MVar (Map AcceptTime Packet)

type PktTimeBuffer = MVar (Seq Packet)
