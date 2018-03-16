{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe (fromMaybe)
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime (..), picosecondsToDiffTime)

parsePkt :: ByteString -> Either String (AcceptTime, Packet)
parsePkt bs = do
  let p = Packet <$> (P.manyTill P.anyChar "B6034" *> P.take 12)
                 <*> (P.take 12 *> parseBids)
                 <*> (P.take 7 *> parseAsks)
                 <**> ((,) <$> (P.take 50 *> parseAcceptTime))
  case P.parseOnly p bs of
    Left _  -> Left "Wrong data and/or information type."  -- many packets do not have data type "B6"...
    Right r -> Right r                                     -- ...and/or information type "03"
      
parseBids :: Parser Bids
parseBids = let toInt = fst . fromMaybe (0, "") . BS.readInt 
                p = P.count 5 $ (flip Quote) <$> (toInt <$> P.take 5)  -- `flip` to print quantities before prices
                                             <*> (toInt <$> P.take 7) 
                toTuple = \[a, b, c, d, e] -> (a, b, c, d, e)
            in (toTuple . reverse) <$> p                               -- bids in reverse order from in packet
      
parseAsks :: Parser Asks
parseAsks = let toInt = fst . fromMaybe (0, "") . BS.readInt 
                p = P.count 5 $ (flip Quote) <$> (toInt <$> P.take 5)
                                             <*> (toInt <$> P.take 7) 
                toTuple = \[a, b, c, d, e] -> (a, b, c, d, e)
            in toTuple <$> p

parseAcceptTime :: Parser AcceptTime
parseAcceptTime = let p = P.count 4 $ fst . fromMaybe (0, "") . BS.readInteger <$> P.take 2
                      toPico = \p -> zipWith3 (\a b c -> a * 10^b * c) p [12, 12, 12, 6] [3600, 60, 1, 1]
                      toUTC  = \p -> p + 32400*10^12  -- UTC+9
                      toTime = \p -> picosecondsToDiffTime $ toUTC $ sum $ toPico p  -- microseconds -> picoseconds
                  in (\p -> AcceptTime $ UTCTime (fromGregorian 2011 2 16) (toTime p)) <$> p  -- convert to UTCTime
