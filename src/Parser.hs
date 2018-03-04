{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Applicative
import           Control.Lens.Each (each)
import           Control.Lens.Operators
import           Control.Lens.Traversal (partsOf)
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.List (transpose)
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (picosecondsToDiffTime)

parsePkt :: ByteString -> Either String (AcceptTime, Packet)
parsePkt bs = do
  let p = Packet <$> (P.manyTill P.anyChar "B6034" *> P.take 12)
                 <*> (P.take 12 *> parseQuotes)
                 <*> (P.take 7 *> parseQuotes)
                 <**> ((,) <$> (P.take 50 *> parseAcceptTime))
  case P.parseOnly p bs of
    Left _  -> Left "Wrong data and/or information type."
    Right r -> Right r
      
parseQuotes :: Parser Quotes
parseQuotes = let toInt = fst . fromMaybe (0, "") . BS.readInt 
                  p = P.count 5 $ (,) <$> (toInt <$> P.take 5)
                                      <*> (toInt <$> P.take 7)
                  flatten  = concat . (\p -> (\(a, b) -> [a, b]) <$> p)
                  toTuple  = \p -> undefined & partsOf each .~ p :: (Int, Int, Int, Int, Int)
                  toQuotes = \p -> (\l -> (toTuple $ head l, toTuple $ head $ tail l)) $ transpose $ chunksOf 2 p
              in toQuotes <$> flatten <$> p

parseAcceptTime :: Parser AcceptTime
parseAcceptTime = let p = P.count 4 $ fst . fromMaybe (0, "") . BS.readInteger <$> P.take 2
                      toPico = \p -> zipWith3 (\a b c -> a^b * c) p [6, 12, 12, 12] [1, 1, 60, 3600]
                      toTime = \p -> picosecondsToDiffTime $ sum $ toPico p
                  in toTime <$> p
