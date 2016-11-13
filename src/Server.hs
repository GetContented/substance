{-# Language OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Snap
  ( Snap, runSnaplet, quickHttpServe
  )
import ServerState ( ServerState, makeServerStateInit )

startServer :: IO ()
startServer = do
  let
    environment = Nothing
    initialServerState = makeServerStateInit []
  (_, snaplet, _) <- runSnaplet environment initialServerState
  quickHttpServe snaplet
