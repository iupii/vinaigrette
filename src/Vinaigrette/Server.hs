{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Vinaigrette.Server
    ( application
    , jsonRequestLogger
    ) where

import Data.Text
import Data.Default.Class (Default(def))
import Control.Monad.Reader
import Network.Wai (Middleware)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.RequestLogger.JSON
import Servant

import Vinaigrette.App
import Vinaigrette.Api
import Vinaigrette.Log


sampleHandler :: AppM Text
sampleHandler = do
    _ <- logMsgA Info "let's do some logging!"
    pure "Hello World"

server :: Env -> Server Api
server env = hoistServer (Proxy @Api) (`runReaderT` env) sampleHandler

application :: Env -> Application
application env = serve (Proxy @Api) (server env)

jsonRequestLogger :: IO Middleware
jsonRequestLogger =
    mkRequestLogger $ def { outputFormat = CustomOutputFormatWithDetails formatAsJSON }
