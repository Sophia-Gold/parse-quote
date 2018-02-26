module Test where

import           Main
import           Capture
import           Parser
import qualified Data.ByteString.Char8 as C
import           System.IO

pktTimeTest :: IO ()
pktTimeTest = do
  contents <- getContents
  p <- pcapParse contents
  C.putStrLn <$> pktOrd pcapStream' p
