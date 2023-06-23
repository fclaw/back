module Servant.Swagger.Internal.Extended (module Internal) where

import Control.Lens
import Data.Swagger
import Servant.API.WebSocket (WebSocketPending)
import Servant.Swagger.Internal as Internal

instance HasSwagger WebSocketPending where
  toSwagger _ = mempty & paths . at "/" ?~ mempty
