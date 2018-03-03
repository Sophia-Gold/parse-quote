{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Applicative
import           Control.Lens.Each (each)
import           Control.Lens.Operators
import           Control.Lens.Traversal (partsOf)
import           Data.Attoparsec (parseOnly)
import           Data.Attoparsec.ByteString.Char8 (Parser)
import qualified Data.Attoparsec.ByteString.Char8 as P
import           Data.Bifunctor (second)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
-- import           Data.Int (Int32)
import           Data.List (transpose)
import           Data.List.Split (chunksOf)
import           Data.Maybe (fromMaybe)
import           Data.Time.Clock (DiffTime, picosecondsToDiffTime)

parsePkt :: ByteString -> Either String (Packet, AcceptTime)
parsePkt bs = do
  let p = Packet <**> ((,) <$>
                       "B6034" *> P.take 12
                       <*> P.take 12 *> parseQuotes
                       <*> P.take 7 *> parseQuotes
                       <*> P.take 50 *> parseAcceptTime)
  case parseOnly p bs of
    Left _  -> Left "Failed parsing packet."
    Right r -> Right r
      
parseQuotes :: Parser Quotes
parseQuotes = let p = P.count 5 $ fst . fromMaybe (0, "") . BS.readInt <$> P.take 5 <*> P.take 7
                  q  = \p -> undefined & partsOf each .~ p :: (Int, Int, Int, Int)
                  toQuotes = \p -> (\l -> (q $ head l, q $ head $ tail l)) $ transpose $ chunksOf 2 p
              in toQuotes <$> p

parseAcceptTime :: Parser DiffTime
parseAcceptTime = let p = P.count 4 $ fst . fromMaybe (0, "") . BS.readInteger <$> P.take 2
                      toPico = \p -> zipWith3 (\a b c -> a^b * c) p [6, 12, 12, 12] [1, 1, 60, 3600]
                      toTime = \p -> picosecondsToDiffTime $ sum $ toPico p
                  in toTime <$> p
