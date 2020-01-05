module Main(main) where

import Remotely
import System.IO

main :: IO ()
main = receiver inc stdin stdout

inc :: Int -> IO Int
inc x = do
  print ("got " <> show x <> ", returning " <> (show (x+1)))
  pure (x+1)
