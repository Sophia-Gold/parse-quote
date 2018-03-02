{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Parser.Types
import           Control.Lens.Each (each)
import           Control.Lens.Operators
import           Control.Lens.Traversal (partsOf)
import           Prelude hiding (take)
import           Data.Attoparsec (parseOnly, Parser)
import           Data.Attoparsec.ByteString.Char8
import           Data.Bifunctor (second)
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (digitToInt)
import           Data.Int (Int32)
import           Data.Time.Clock (picosecondsToDiffTime)

parsePkt :: ByteString -> Either String (AcceptTime, Packet)
parsePkt bs = case parse (skipMany1 "B6034" *> parseICode) bs of
  Fail _ _ _  -> Left "Failed parsing issue code."
  Partial _   -> Left "Failed parsing issue code." 
  Done rest i -> case parse (take 7 *> parseBids) rest of
    Fail _ _ _  -> Left "Failed parsing bid quotes."
    Partial _   -> Left "Failed parsing bid quotes."
    Done rest b -> case parse (take 7 *> parseAsks) rest of
      Fail _ _ _  -> Left "Failed parsing ask quotes."
      Partial _   -> Left "Failed parsing ask quotes."
      Done rest a -> case parse (take 50 *> parseAcceptTime) rest of
        Fail _ _ _  -> Left "Failed parsing accept time."
        Partial _   -> Left "Failed parsing accept time."
        Done _ at   -> Right (at, Packet i b a)

parseICode :: ByteString -> Either String (ByteString, IssueCode)
parseICode = \bs ->
  case parseOnly $ take 12 of
    Done rest result -> (rest, result)
    _                -> "Failed parsing issue code." 

parseAcceptTime :: ByteString -> Parser AcceptTime
parseAcceptTime = \bs -> let p = (toInteger . digitToInt) <$> chunk2 $ take 8 where
                               chunk2 [] = []
                               chunk2 l = take 2 l : (chunk2 $ drop 2 l)
                             toPico = \t -> zipWith3 (\a b c -> a^b * c) t [6, 12, 12, 12] [1, 1, 60, 3600]
                             toTime = \t -> picosecondsToDiffTime $ sum $ toPico t
                         in second toTime p
  -- case parse p of
  --   Fail bs ctxs msg -> putStrLn msg
  --   Partial cont     -> putStrLn "Failed parsing issue code."
  --   Done bs result   -> picosecondsToDiffTime $ sum $ toPico p
      
parseBids :: ByteString -> Either String Bids
parseBids bs = pEven 0 where
  pEven 12 = case parse pg Even bs of
               Fail bs ctxs msg -> Left msg
               Partial cont     -> Left "Failed parsing bid."
               Done bs result   -> let b = splitAt 6 $ digitToInt result
                                   in Right ((undefined & partsOf each
                                              .~ (snd result) :: (Int32,Int32,Int32,Int32))
                                            , (undefined & partsOf each
                                               .~ (fst result) :: (Int32,Int32,Int32,Int32)))
  pEven i = (\bs -> take 5 bs) (i + 1) 
  pPOdd i = (\bs -> take 7 bs) (i + 1) 
    
parseAsks :: ByteString -> Either String Asks
parseAsks bs = pEven 0 where
  pEven 12 = case parse pEven bs of
               Fail bs ctxs msg -> Left msg
               Partial cont     -> Left "Failed parsing bid."
               Done bs result   -> let a = splitAt 6 $ digitToInt resultRight
                                   in Right ((undefined & partsOf each
                                              .~ (snd result) :: (Int32,Int32,Int32,Int32))
                                            , (undefined & partsOf each
                                               .~ (fst result) :: (Int32,Int32,Int32,Int32)))
  pEven i = (\bs -> take 5 bs) *> pOdd (i + 1)
  pOdd i  = (\bs -> take 7 bs) *> pEven (i + 1)
