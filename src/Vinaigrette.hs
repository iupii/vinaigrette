{-# LANGUAGE OverloadedStrings #-}

module Vinaigrette (main) where

import Network.Wai.Handler.Warp as Warp
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet)
import Vinaigrette.App
import Vinaigrette.Log
import Vinaigrette.Server



port :: Int
port = 8080

main :: IO ()
main = do
  warpLogger <- jsonRequestLogger
  appLogger <- newStdoutLoggerSet defaultBufSize

  logMsg appLogger Info "My app starting up!"

  let env = Env appLogger
      warpSettings = Warp.defaultSettings
      portSettings = Warp.setPort port warpSettings
      settings = Warp.setTimeout 55 portSettings
  Warp.runSettings settings $ warpLogger $ application env
