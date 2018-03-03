module Main where

import           Capture
import           Capture.Types
import           Parser
import           Parser.Types
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map (empty)
import qualified Data.Sequence as S (empty)
import           System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  acceptOrdBuf <- newMVar Map.empty
  pktOrderBuf  <- newMVar S.empty
  case head args of
    "-r" -> readPkts (args !! 1) acceptOrdBuf *> BS.putStrLn <$> readMVar acceptOrdBuf
    _    -> readPkts (args !! 0) pktOrderBuf  *> BS.putStrLn <$> readMVar pktOrderBuf
