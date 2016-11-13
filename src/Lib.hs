module Lib
    ( startServer
    ) where

import qualified Server

startServer :: IO ()
startServer = Server.startServer
