{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module ServerState
    ( ServerState
    , makeServerStateInit
    ) where

import Snap ( addRoutes, writeBS )
import Snap.Snaplet
  ( Snaplet, subSnaplet, SnapletInit, makeSnaplet, nestSnaplet )
import Snap.Snaplet.Heist ( Heist, HasHeist(heistLens), heistInit )
import Control.Lens ( makeLenses )

data ServerState = ServerState {
  _heist :: Snaplet ( Heist ServerState )
}
makeLenses ''ServerState

instance HasHeist ServerState where
  heistLens = subSnaplet heist

makeServerStateInit :: SnapletInit ServerState ServerState
makeServerStateInit =
  let
    name = "Substance"
    desc = "Substance System V1"
    maybeFilepath = Nothing
  in
    makeSnaplet name desc maybeFilepath $ do
      heistData <- nestSnaplet "heist" heist $ heistInit "templates"
      addRoutes [ ("", writeBS "Welcome to Substance - Catchall route handler")]
      return $ ServerState { _heist = heistData }