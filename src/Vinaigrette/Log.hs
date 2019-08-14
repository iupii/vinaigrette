{-# LANGUAGE DeriveGeneric #-}

module Vinaigrette.Log
    ( Level (..)
    , logMsg
    , logMsgA
    ) where

import Data.Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Aeson
import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import GHC.Generics
import System.Log.FastLogger (ToLogStr(..), LoggerSet, pushLogStrLn)

import Vinaigrette.App


data Level = Info | Debug | Error
    deriving (Eq, Show, Generic)

instance FromJSON Level
instance ToJSON Level where
    toEncoding = genericToEncoding defaultOptions

data Message = Message
    { lmMessage :: !Text
    , lmTimestamp :: !UTCTime
    , lmLevel :: !Level
    } deriving (Eq, Show, Generic)

instance FromJSON Message
instance ToJSON Message where
    toEncoding = genericToEncoding defaultOptions

instance ToLogStr Message where
    toLogStr = toLogStr . encode

logMsgA :: Level -> Text -> AppM (IO ())
logMsgA level message = do
    logSet <- asks envLogger
    timestamp <- liftIO getCurrentTime
    let msg = Message { lmMessage = message
                      , lmTimestamp = timestamp
                      , lmLevel = level
                      }
    pure $ pushLogStrLn logSet $ toLogStr msg

logMsg :: LoggerSet -> Level -> Text -> IO ()
logMsg logSet level message = do
    timestamp <- liftIO getCurrentTime
    let msg = Message { lmMessage = message
                      , lmTimestamp = timestamp
                      , lmLevel = level
                      }
    pushLogStrLn logSet $ toLogStr msg