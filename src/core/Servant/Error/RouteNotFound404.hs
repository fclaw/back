{-# LANGUAGE OverloadedStrings     #-}

module Servant.Error.RouteNotFound404 (formatter) where

import Servant.Server (err404, errBody, ServerError)
import Network.Wai (rawPathInfo, Request)
import Control.Lens.Iso.Extended (bytesLazy)
import Control.Lens (from, (^.))

formatter :: Request -> ServerError
formatter req = err404 { errBody = ("Not found path: " <> rawPathInfo req)^.from bytesLazy }
