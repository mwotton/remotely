{-# LANGUAGE ScopedTypeVariables #-}
module Main(main) where

import Remotely
-- import System.IO
import System.Environment
import Control.Exception
import Text.Read

main :: IO ()
main = do
  [hostname,path] <- getArgs
  (r ::Either RemoteError ()) <- remotely hostname path (printOut, maybeGetline)
  print ("finally", r)

printOut :: Int -> IO Status
printOut x = print ("printout called" <> show x) >> pure NotDone

maybeGetline :: IO (Maybe Int)
maybeGetline = do
  putStrLn "waiting for input"
  x <- try getLine
  case x of
    Left (_ :: IOError) -> print "ending input" >> pure Nothing
    Right r -> print ("got " <> show r) >> pure (readMaybe r)
