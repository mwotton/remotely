{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
module Remotely
       ( remotely
       , RemoteError(..)
       , Status(..)
       ) where

import Data.Serialize
import Control.Monad.IO.Class(MonadIO)
import GHC.Generics(Generic)

data Status = Done | NotDone
  deriving Generic
instance Serialize Status

data RemoteError
  = CannotConnect
  | ParseError
  | ExecutableError
  deriving Generic

instance Serialize RemoteError

type Hostname = String

remotely :: (Serialize a, Serialize b, MonadIO m)
         => Hostname -> FilePath -> (a -> m Status, m (Maybe b))  -> m (Either RemoteError ())
remotely hostname remotePath (sender,receiver) = undefined
