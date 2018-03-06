module Main where

import           Capture
import           Control.Concurrent.Async.Timer
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map.Strict as Map
import           System.Environment (getArgs)
import           TextShow (printT)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "-r" -> do
      buf <- newMVar Map.empty
      readPkts (args !! 1) (enqueueAcceptOrd buf)
      let conf = timerConfSetInitDelay 4000 $ timerConfSetInterval 4000 $ defaultTimerConf -- 4s delay
      withAsyncTimer conf $ \ timer -> do
        oldBuf <- takeMVar buf
        let m' = Map.assocs oldBuf
            m = splitAt (length m' `div ` 4 * 3) m'
        newBuf <- putMVar buf (Map.fromList $ snd m)
        sequence_ ((printT . (\(at,(pt,p)) -> (pt,at,p))) <$> fst m)
        timerWait timer
    _    -> do
      buf <- newMVar Map.empty
      readPkts (args !! 0) (enqueuePktOrd buf)
      let conf = timerConfSetInitDelay 4000 $ timerConfSetInterval 4000 $ defaultTimerConf -- 4s delay
      withAsyncTimer conf $ \ timer -> do
        oldBuf <- takeMVar buf
        let m' = Map.assocs oldBuf
            m = splitAt (length m' `div ` 4 * 3) m'
        newBuf <- putMVar buf (Map.fromList $ snd m)
        sequence_ ((printT . (\(pt,(at,p)) -> (pt,at,p))) <$> fst m)
        timerWait timer
