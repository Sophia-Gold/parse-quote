module Main where

import           Capture
import           Capture.Types
import           Parser
import           Parser.Types
import           Control.Monad.State.Lazy (execState)
import qualified Data.ByteString.Char8 as BS
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  acceptOrdBuf <- AcceptTimeBuffer
  pktOrderBuf  <- PktTimeBuffer 
  case head args of
    "-r" -> readPkts (args !! 1) acceptOrdBuf *> BS.putStrLn <$> execState acceptOrdBuf
    _    -> readPkts (args !! 0) pktOrderBuf  *> BS.putStrLn <$> execState pktOrderBuf
