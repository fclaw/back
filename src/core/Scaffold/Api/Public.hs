{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.Public (PublicApi (..)) where

import Servant.API.Extended (type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal.Extended ()

newtype PublicApi route = PublicApi
  { _publicApiGetServerInfo ::
      route
        :- "server"
          :> "info"
          :> WebSocketPending
  }
  deriving stock (Generic)
