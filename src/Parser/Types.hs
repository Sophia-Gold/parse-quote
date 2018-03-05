{-# LANGUAGE TemplateHaskell #-}

module Parser.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (DiffTime)
import TextShow.Data.ByteString
import TextShow.TH

type PktTime = DiffTime
type AcceptTime = DiffTime
$(deriveTextShow ''DiffTime)

data Quote = Quote {
    quantity :: Int
  , price :: Int
  }
$(deriveTextShow ''Quote)

type Bids = (Quote, Quote, Quote, Quote, Quote)
type Asks = (Quote, Quote, Quote, Quote, Quote)

data Packet = Packet {
    issue_code :: ByteString
  , bids :: Bids
  , asks :: Asks
  }
$(deriveTextShow ''Packet)
