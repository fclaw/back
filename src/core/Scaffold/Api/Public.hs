{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Public (PublicApi (..)) where

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended (type (:>) )
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

newtype PublicApi route =
        PublicApi {
          _publicApiGetServerInfo
          :: route
          :- "server"
          :> "info"
          :> WebSocketPending
        } deriving stock Generic