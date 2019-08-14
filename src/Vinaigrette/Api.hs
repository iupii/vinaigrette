{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Vinaigrette.Api
    ( Api
    ) where

import Data.Text
import Servant.API

type Api = Get '[PlainText] Text
