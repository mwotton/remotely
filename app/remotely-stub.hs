module Main(main) where

import Remotely
import System.IO
import Debug.Trace

main :: IO ()
main = receiver inc stdin stdout

inc :: Int -> IO Int
inc x = do
  traceM ("got " <> show x <> ", returning " <> (show (x+1)))
  pure (x+1)
