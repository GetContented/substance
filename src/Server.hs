{-# Language OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Snap
  ( Snap, runSnaplet, quickHttpServe
  )
import ServerState ( ServerState, makeServerStateInit )
import Routes ( routes )

startServer :: IO ()
startServer = do
  let
    environment = Nothing
    initialServerState = makeServerStateInit routes
  (_, snaplet, _) <- runSnaplet environment initialServerState
  quickHttpServe snaplet
