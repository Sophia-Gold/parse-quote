module Main where

import           Capture
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           TextShow (printT)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "-r" -> do   -- ordered by quote accept time
      buf <- newMVar Map.empty
      readPkts (args !! 1) (enqueueAcceptOrd buf)
      buf <- takeMVar buf
      sequence_ ((printT . (\(at,(pt,p)) -> (pt,at,p))) <$> Map.assocs buf)  -- print packet receipt time first
    _    -> do   -- default to ordering by packet receipt time
      buf <- newMVar Map.empty
      readPkts (args !! 0) (enqueuePktOrd buf)
      buf <- takeMVar buf
      sequence_ ((printT . (\(pt,(at,p)) -> (pt,at,p))) <$> Map.assocs buf)
