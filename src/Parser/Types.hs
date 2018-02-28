module Parser.Types where

import Data.ByteString (ByteString)
import Data.Time.Clock (DiffTime)

type PktTime = DiffTime
type AcceptTime = DiffTime

type IssueCode = ByteString

type BidPrices = (Int, Int, Int, Int, Int)
type BidQuants = (Int, Int, Int, Int, Int)

type AskPrices = (Int, Int, Int, Int, Int)
type AskQuants = (Int, Int, Int, Int, Int)

type Bids = (BidQuants, BidPrices)
type Asks = (AskQuants, AskPrices)

data Packet = Packet {
    pktTime :: PktTime
  , acceptTime :: AcceptTime
  , issueCode :: IssueCode
  , bids :: Bids
  , asks :: Asks
  } deriving (Show)
