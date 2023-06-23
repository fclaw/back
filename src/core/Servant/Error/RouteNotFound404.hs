{-# LANGUAGE OverloadedStrings #-}

module Servant.Error.RouteNotFound404 (formatter) where

import Control.Lens (from, (^.))
import Control.Lens.Iso.Extended (bytesLazy)
import Network.Wai (Request, rawPathInfo)
import Servant.Server (ServerError, err404, errBody)

formatter :: Request -> ServerError
formatter req = err404 {errBody = ("Not found path: " <> rawPathInfo req) ^. from bytesLazy}
