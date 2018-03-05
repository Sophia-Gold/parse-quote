module Capture.Types where

import Parser.Types
import Control.Concurrent.MVar (MVar)
import Data.Map.Strict (Map)
  
type AcceptTimeBuffer = MVar (Map AcceptTime Packet)

type PktTimeBuffer = MVar (Map PktTime Packet)
