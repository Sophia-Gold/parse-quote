module Parser.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (DiffTime)

type PktTime = DiffTime
type AcceptTime = DiffTime

type IssueCode = ByteString

type Prices = (Int, Int, Int, Int, Int)
type Quants = (Int, Int, Int, Int, Int)
type Quotes = (Quants, Prices)

data Packet = Packet {
    issueCode :: IssueCode
  , bids :: Quotes
  , asks :: Quotes
  } deriving (Show)
