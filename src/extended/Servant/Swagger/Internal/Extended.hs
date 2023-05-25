module Servant.Swagger.Internal.Extended (module Internal) where 

import Servant.Swagger.Internal as Internal
import Servant.API.WebSocket (WebSocketPending)
import Control.Lens
import Data.Swagger

instance HasSwagger WebSocketPending where
  toSwagger _ = mempty & paths . at "/" ?~ mempty