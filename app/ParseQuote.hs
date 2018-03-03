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
  case head args of
    "-r" -> do
      acceptOrdBuf <- newMVar Map.empty
      readPkts (args !! 1) (enqueueAcceptOrd acceptOrdBuf)
      acceptOrdBuf' <- readMVar acceptOrdBuf
      putStrLn $ show $ acceptOrdBuf'
      return ()
    _    -> do
      pktOrderBuf <- newMVar S.empty
      readPkts (args !! 0) (enqueuePktOrd pktOrderBuf)
      pktOrderBuf' <- readMVar pktOrderBuf
      putStrLn $ show $ pktOrderBuf'
