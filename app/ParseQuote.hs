module Main where

import           Capture
import           Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as BS
import           Data.Foldable (toList)
import qualified Data.Map.Strict as Map (empty)
import qualified Data.Sequence as S (empty)
import           System.Environment (getArgs)
import           TextShow (printT)

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "-r" -> do
      buf <- newMVar Map.empty
      readPkts (args !! 1) (enqueueAcceptOrd buf)
      buf <- takeMVar buf
      sequence_ $ printT <$> (toList $ buf)
    _    -> do
      buf <- newMVar S.empty
      readPkts (args !! 0) (enqueuePktOrd buf)
      buf <- takeMVar buf
      sequence_ $ printT <$> (toList $ buf)
