module Main(main) where

import Remotely
import System.IO
import Debug.Trace

main :: IO ()
main = receiver double stdin stdout

double :: String -> IO String
double x = do
  let y = x <> x
  traceM ("got " <> show x <> ", returning " <> (show y))
  pure y

inc :: Int -> IO Int
inc x = do
  traceM ("got " <> show x <> ", returning " <> (show (x+1)))
  pure (x+1)
