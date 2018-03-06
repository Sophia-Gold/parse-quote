{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Parser.Types where

import Data.ByteString.Char8 (ByteString)
import Data.Time.Clock (DiffTime)
import Data.Semigroup ((<>))
import TextShow
import TextShow.Data.ByteString
import TextShow.Data.Time
import TextShow.TH

newtype PktTime = PktTime DiffTime deriving (Eq, Ord)
newtype AcceptTime = AcceptTime DiffTime deriving (Eq, Ord)

instance TextShow PktTime where
  showb (PktTime t) = "Packet-Time: " <> showb t

instance TextShow AcceptTime where
  showb (AcceptTime t) = " Accept-Time: " <> showb t

data Quote = Quote {
    quantity :: Int
  , price :: Int
  }

instance TextShow Quote where
  showb (Quote q p) = " " <> showb q <> "@" <> showb p

type Bids = (Quote, Quote, Quote, Quote, Quote)
type Asks = (Quote, Quote, Quote, Quote, Quote)

data Packet = Packet {
    issueCode :: ByteString
  , bids :: Bids
  , asks :: Asks
  }

instance TextShow Packet where
  showb (Packet i b a) = " Issue-Code: " <> showb i <> ", "
                      <> "Bids:" <> showb b <> ", "
                      <> "Asks:" <> showb a
