module Parser.Types where

import Data.ByteString (ByteString)
import Data.Time.Clock (DiffTime)

type PktTime = DiffTime
type AcceptTime = DiffTime

type IssueCode = ByteString

type BidPrices = (Int32, Int32, Int32, Int32, Int32)
type BidQuants = (Int32, Int32, Int32, Int32, Int32)

type AskPrices = (Int32, Int32, Int32, Int32, Int32)
type AskQuants = (Int32, Int32, Int32, Int32, Int32)

type Bids = (BidQuants, BidPrices)
type Asks = (AskQuants, AskPrices)

data Packet = Packet {
    pktTime :: PktTime
  , acceptTime :: AcceptTime
  , issueCode :: IssueCode
  , bids :: Bids
  , asks :: Asks
  } deriving (Show)
