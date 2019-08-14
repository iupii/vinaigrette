module Vinaigrette.App
    ( Env (..)
    , AppM
    ) where

import Control.Monad.Reader
--import Data.Pool (Pool)
--import Hasql.Connection (Connection)
import Servant (Handler)
import System.Log.FastLogger (LoggerSet)


--type DbPool = Pool Connection

data Env = Env
    { --envDbPool :: !DbPool,
     envLogger :: LoggerSet
    }

type AppM = ReaderT Env Handler
