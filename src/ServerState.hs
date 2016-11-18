{-# LANGUAGE TemplateHaskell, OverloadedStrings, FlexibleInstances #-}

module ServerState
    ( ServerState
    , makeServerStateInit
    , ServerStateHandler
    ) where

import Snap ( addRoutes, writeBS )
import Snap.Snaplet
  ( Snaplet, subSnaplet, SnapletInit, makeSnaplet, nestSnaplet, Handler, with )
import Snap.Snaplet.Heist ( Heist, HasHeist(heistLens), heistInit )
import Control.Lens ( makeLenses )
import Data.ByteString.Char8 ( ByteString )
import Control.Monad.State ( get )
import Snap.Snaplet.Groundhog.Postgresql
  ( HasGroundhogPostgres, GroundhogPostgres, getGroundhogPostgresState
  , initGroundhogPostgres )
import Data.Text

type ServerStateHandler =
  Handler ServerState ServerState ()

data ServerState = ServerState {
    _heist :: Snaplet ( Heist ServerState )
  , _db :: Snaplet GroundhogPostgres
}
makeLenses ''ServerState

instance HasHeist ServerState where
  heistLens = subSnaplet heist

instance HasGroundhogPostgres (Handler b ServerState) where
  getGroundhogPostgresState = with db Control.Monad.State.get


makeServerStateInit :: [(ByteString, ServerStateHandler)]
                    -> SnapletInit ServerState ServerState
makeServerStateInit routes =
  let
    name = "Substance"
    desc = "Substance System V1"
    maybeFilepath = Nothing
  in
    makeSnaplet name desc maybeFilepath $ do
      heistData <- nestSnaplet "heist" heist $ heistInit "templates"
      dbData <- nestSnaplet "db" db initGroundhogPostgres
      addRoutes routes
      return $ ServerState {
          _heist = heistData
        , _db = dbData
      }
