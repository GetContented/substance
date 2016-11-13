{-# LANGUAGE TemplateHaskell #-}

module ServerState
    ( ServerState
    , heist
    ) where

import Snap.Snaplet (Snaplet, subSnaplet)
import Snap.Snaplet.Heist (Heist, HasHeist(heistLens))
import Control.Lens (makeLenses)

data ServerState = ServerState {
  _heist :: Snaplet (Heist ServerState)
}
makeLenses ''ServerState

instance HasHeist ServerState where
  heistLens = subSnaplet heist
