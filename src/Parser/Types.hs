module Parser.Types where

import Data.ByteString (ByteString)
import Data.Time.Clock (UTCTime)

type PktTime = UTCTime

type AcceptTime = UTCTime

type IssueCode = ByteString

type Bids = (Double, Double, Double, Double, Double)

type Asks = (Double, Double, Double, Double, Double)

data Packet = Packet {
    pktTime :: PktTime
  , acceptTime :: AcceptTime
  , issueCode :: IssueCode
  , bids :: Bids
  , asks :: Asks
  } deriving (Show)
