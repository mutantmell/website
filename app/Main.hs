module Main where

import qualified Server (handler, API)

import           Data.Proxy ( Proxy(Proxy) )
import qualified Network.Wai.Handler.Warp as Warp
import           Servant ( serve )


main :: IO ()
main = Warp.run 8080 (serve (Proxy @Server.API) Server.handler)
