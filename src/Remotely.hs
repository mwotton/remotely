{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
module Remotely
       ( remotely
       , receiver
       , RemoteError(..)
       , Status(..)
       ) where

import Conduit
import Control.Monad.IO.Class(MonadIO, liftIO)
import GHC.Generics(Generic)
import Control.Concurrent.Async.Lifted(concurrently)
import System.Process(CreateProcess(..),proc,StdStream(..), createProcess)
import Debug.Trace
import Data.Maybe(fromMaybe)
import Control.Monad.Trans.Control(MonadBaseControl)
import System.IO
import Data.Store
import Data.Store.Streaming

import Control.Exception.Lifted(try, Exception)

data Status = Done | NotDone
  deriving Generic
instance Store Status

data RemoteError
  = CannotConnect
  | ParseError
  | ExecutableError
  deriving (Generic,Show)
instance Store RemoteError
instance Exception RemoteError

type Hostname = String

remotely :: forall m a b . (Store a, Store b, Show a, Show b, MonadIO m, MonadUnliftIO m, MonadBaseControl IO m)
         => Hostname -> FilePath
         -> (a -> m Status, m (Maybe b))
         -> m (Either RemoteError ())
remotely hostname remotePath (sendFunc,receiveFunc) = do
  let process = (proc "ssh" [hostname, "-e", "none", remotePath])
        { std_out = CreatePipe
        , std_in  = CreatePipe
        , std_err = Inherit
        }
  (mhin, mhout, _, _phandle) <- liftIO $ createProcess process

  -- statically, we know these are Justs, and don't want to incur the
  -- MonadFail constraint.
  let hin = fromMaybe (error "impossible") mhin
      hout = fromMaybe (error "impossible") mhout
  liftIO $ hSetBuffering hin NoBuffering
  liftIO $ hSetBuffering hout NoBuffering
  inspect <$> concurrently (receiveLoop hout) (sendLoop hin)

  where
    inspect :: (Either RemoteError (), Either RemoteError ()) -> Either RemoteError ()
    inspect (Left x, _) = Left x
    inspect (_ ,Left x) = Left x
    inspect _  = Right ()

    watch name = mapMC (\x -> trace (name <> ": " <> show x) (pure x))
    receiveLoop h = do
      traceM "router recv loop"
      r <- try $ runConduitRes (sourceHandle h
        .| watch "recv loop"
        .| conduitDecode Nothing
        .| watch "recv loop2"
        .| sink)
      traceM "router recv loop end"
      pure r
    sink :: ConduitM (Message a) Void (ResourceT m) ()
    sink = do
      traceM "sink!"
      await >>=  maybe (pure ()) (\(Message m') -> do
        traceM ("sinkloop: " <> show (Message m'))
        lift (lift $ sendFunc m') >>= \case
          Done -> pure ()
          NotDone -> sink)

    sendLoop :: Handle -> m (Either RemoteError ())
    sendLoop h = do
      r <- try $ runConduit
        (source
          .| mapMC (\x -> trace ("sender1: " <> show x) (pure x))
          .| conduitEncode
          .| mapMC (\x -> trace ("sender2: " <> show x) (pure x))
          .| sinkHandle h)
      trace "finishing sendloop" (pure r)
    source = maybe (pure ()) (\x -> do
                                 yield (Message x)
                                 traceM ("sourceloop: " <> show (Message x))
                                 )
        =<< lift receiveFunc

-- TODO some way of quitting from outside (though maybe can just kill the thread?)
receiver :: forall a b m .
           (Show a, Show b, Store a, Store b, MonadIO m, MonadUnliftIO m)
         => (a -> m b)
         -> Handle
         -> Handle
         -> m ()
receiver f hin hout = do
  trace "starting receiver" (pure ())
  liftIO $ hSetBuffering hin NoBuffering
  liftIO $ hSetBuffering hout NoBuffering
  runConduitRes
    $ sourceHandle hin
    .| mapMC (\x -> trace ("decoder1: " <> show x) (pure x))
    .| conduitDecode Nothing
    .| mapMC (\x -> trace ("decoder2: " <> show x) (pure x))
    .| mapMC (lift . fmap Message . f . fromMessage)
    .| mapMC (\x -> trace ("decoder3: " <> show x) (pure x))
    .| conduitEncode
    .| mapMC (\x -> trace ("decoder4: " <> show x) (pure x))
    .| sinkHandle hout
  traceM "receiver done"
