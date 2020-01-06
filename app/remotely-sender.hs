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
  pure ()

printOut :: String -> IO Status
printOut x = print ("printout called: " <> show x) >> pure Done

maybeGetline :: IO (Maybe String)
maybeGetline = do
  x <- try getLine
  case x of
    Left (_ :: IOError) -> pure Nothing
    Right r -> pure (Just r)
