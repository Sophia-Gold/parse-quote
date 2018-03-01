module Main where

import           Capture
import           Capture.Types
import           Parser
import           Parser.Types
import qualified Data.ByteString.Char8 as C
import           Data.IntMap
import           Data.List (sortOn)
import           Data.Sequence as S
import           Data.Time.Clock
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case head args of
    "-r" -> readPkts $ args !! 1
    _    -> readPkts $ args !! 0
    -- "-r" -> C.putStrLn <$> runAcceptOrd pcapStreamAcceptTime'
    -- _    -> C.putStrLn <$> runPktOrd pcapStreamPktTime'
