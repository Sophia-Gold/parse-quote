{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Lens.Tuple
import           Data.Attoparsec (parseOnly, Parser)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Char (digitToInt)
import           Data.List (chunksOf)
import           Data.Time.Clock (picosecondsToDiffTime)

parsePkt :: BS.ByteString -> Either String (AcceptTime, Packet)
parsePkt bs = let p = skipMany1 "B6034"
                   *> parseICode
                   *> skipMany1 $ take 12
                   *> parseBids
                   *> skipMany1 $ take 7
                   *> parseAsks
                   *> skipMany1 $ take 50
                   *> parseAcceptTime
              in case parseOnly p bs of
                   Left err       -> Left err
                   Right i b a at -> Right (at, Packet i b a)

parseICode :: BS.ByteString -> Parser IssueCode
parseICode = \bs -> take 12
  -- case parse $ take 12 of
  --   Fail bs ctxs msg -> putStrLn msg
  --   Partial cont     -> putStrLn "Failed parsing issue code."
  --   Done bs result   -> result

parseAcceptTime :: BS.ByteString -> Parser AcceptTime
parseAcceptTime = \bs -> let p = (toInteger . digitToInt) <$> chunksOf 2 $ take 8
                             toPico = \t -> zipWith3 (\a b x -> a^b * c) t [6, 12, 12, 12] [1, 1, 60, 3600]
                             toTime = \t -> picosecondsToDiffTime $ sum $ toPico t
                         in second toTime p
  -- case parse p of
  --   Fail bs ctxs msg -> putStrLn msg
  --   Partial cont     -> putStrLn "Failed parsing issue code."
  --   Done bs result   -> picosecondsToDiffTime $ sum $ toPico p
      
parseBids :: BS.ByteString -> Either String Bids
parseBids bs = go p 0 where
  go p 12 = case parse p bs of
              Fail bs ctxs msg -> Left msg
              Partial cont     -> Left "Failed parsing bid."
              Done bs result   -> let b = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[]) $ digitToInt <$> result
                                  in Right $ Bids (BidQuants <$> snd b) (BidPrices <$> fst b)
  go p i  = case even i of
              True  -> p (p *> take 5) (i + 1)
              False -> p (p *> take 7) (i + 1)
    
parseAsks :: BS.ByteString -> Either String Asks
parseAsks bs = go p 0 where
  go p 12 = case parse p bs of
              Fail bs ctxs msg -> Left msg
              Partial cont     -> Left "Failed parsing bid."
              Done bs result   -> Right $ Asks (AskPrices <$> result) (AskQuants <$> result)
  go p i  = case even i of
              True  -> p (p *> take 5) (i + 1)
              False -> p (p *> take 7) (i + 1)
