module Capture.Types where

import Parser.Types
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
  
type AcceptTimeBuffer = MVar (Map AcceptTime Packet)

type PktTimeBuffer = MVar (Seq Packet)
