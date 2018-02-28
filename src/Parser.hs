{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Lens.Tuple
import           Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString.Char8 as B
import           Data.Char (digitToInt)
import           Data.Time.Clock (picoSecondsToDiffTime

parsePkt :: ByteString -> Packet
parsePkt bs = let p = parse pktParser bs in
  case p of
    Fail bs ctxs msg -> putStrLn msg
    Partial cont -> append result cont  
    Done bs result -> result

parseICode :: ByteString -> IssueCode
parseICode = \bs ->
  case parse $ P.take 12 of
    Fail bs ctxs msg -> putStrLn msg
    Partial cont     -> putStrLn "Failed parsing issue code."
    Done bs result   -> result

parseAcceptTime :: ByteString -> AcceptTime
parseAcceptTime = \bs ->
  case parse $ P.take 8 of
    Fail bs ctxs msg -> putStrLn msg
    Partial cont     -> putStrLn "Failed parsing issue code."
    Done bs result   -> let t = toInteger result
                        in picoSecondsToDiffTime ((B.drop 6 t)^6 +
                                                  (B.take 2 $ B.drop 4 t)^12 +
                                                  ((B.take 2 $ B.drop 6 t)^12 * 60) +
                                                  (B.take 2 t)^12 * 3600)

parseBids :: ByteString -> Either String Bids
parseBids bs = go p 0 where
  go p 12 = case parse p bs of
              Fail bs ctxs msg -> Left msg
              Partial cont     -> Left "Failed parsing bid."
              Done bs result   -> let b = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[]) $ digitToInt <$> result
                                  in Right $ Bids (BidQuants <$> snd b) (BidPrices <$> fst b)
  go p i  = case even i of
              True  -> p (p *> P.take 5) (i + 1)
              False -> p (p *> P.take 7) (i + 1)
    
parseAsks :: ByteString -> Either String Asks
parseAsks bs = go p 0 where
  go p 12 = case parse p bs of
              Fail bs ctxs msg -> Left msg
              Partial cont     -> Left "Failed parsing bid."
              Done bs result   -> Right $ Asks (AskPrices <$> result) (AskQuants <$> result)
  go p i  = case even i of
              True  -> p (p *> P.take 5) (i + 1)
              False -> p (p *> P.take 7) (i + 1)
