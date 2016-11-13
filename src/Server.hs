{-# Language OverloadedStrings #-}

module Server
    ( startServer
    ) where

import Snap (Snap, writeBS, quickHttpServe)


startServer :: IO ()
startServer = quickHttpServe site

site :: Snap ()
site =
  writeBS "Server now serving basic text for breakfast!"