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

remotely :: forall m a b . (Store a, Store b, MonadIO m, MonadUnliftIO m, MonadBaseControl IO m)
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

-- useful debugging function
--     watch name = mapMC (\x -> trace (name <> ": " <> show x) (pure x))

    receiveLoop h = try $ runConduitRes
      $ sourceHandle h.| conduitDecode Nothing .| sink

    sink :: ConduitM (Message a) Void (ResourceT m) ()
    sink = await >>=  maybe (pure ()) (\(Message m') ->
        lift (lift $ sendFunc m') >>= \case
          Done -> pure ()
          NotDone -> sink)

    sendLoop :: Handle -> m (Either RemoteError ())
    sendLoop h = try $ runConduit $
                 source .| conduitEncode .| sinkHandle h

    source = maybe (pure ()) (yield . Message)
             =<< lift receiveFunc

-- TODO some way of quitting from outside (though maybe can just kill the thread?)
receiver :: forall a b m .
           (Store a, Store b, MonadIO m, MonadUnliftIO m)
         => (a -> m b)
         -> Handle
         -> Handle
         -> m ()
receiver f hin hout = do
  liftIO $ hSetBuffering hin NoBuffering
  liftIO $ hSetBuffering hout NoBuffering
  runConduitRes
    $ sourceHandle hin
    .| conduitDecode Nothing
    .| mapMC (lift . fmap Message . f . fromMessage)
    .| conduitEncode
    .| sinkHandle hout
