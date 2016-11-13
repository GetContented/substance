{-# LANGUAGE OverloadedStrings #-}

module Routes
  ( routes
  ) where

import Snap.Snaplet
  ( Handler )
import Data.ByteString.Char8 ( ByteString )
import ServerState ( ServerStateHandler )
import Snap ( writeBS )


routes :: [(ByteString, ServerStateHandler)]
routes =
  [ ("/about", writeBS "Welcome to substance - This is the about route")
  , ("", writeBS "Welcome to Substance - Catchall route") ]
