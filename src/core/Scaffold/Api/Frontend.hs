{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.Frontend (FrontendApi (..)) where

import qualified Data.Text as T
import Scaffold.Api.Controller.Frontend.GetCookies (Cookie)
import Scaffold.Api.Controller.Frontend.GetMeta (Meta)
import Scaffold.Api.Controller.Frontend.Init (Init)
import Scaffold.Api.Controller.Frontend.Log (FrontendLogRequest)
import Scaffold.Api.Controller.Frontend.Translate hiding (controller)
import Scaffold.Transport.Id
import Scaffold.Transport.Response (Response)
import Servant.API.Extended
import Servant.API.Generic (Generic)

data FrontendApi route = FrontendApi
  { _frontendApiLog ::
      route
        :- "log"
          :> ReqBody '[JSON] FrontendLogRequest
          :> Put '[JSON] (Response ()),
    _frontendApiInit ::
      route
        :- "init"
          :> QueryParam' '[Optional, Strict] "browserIdent" (Id "browserIdent")
          :> Get '[JSON] (Response Init),
    _frontendApiTranslate ::
      route
        :- "translate"
          :> Capture "resource" Resource
          :> Capture "lang" Lang
          :> QueryParam' '[Optional, Strict] "location" Location
          :> Get '[JSON] (Response Translation),
    _frontendApiGetCookies ::
      route
        :- "cookies"
          :> Get '[JSON] (Response [Cookie]),
    _frontendApiGetMeta ::
      route
        :- "meta"
          :> QueryParam' '[Optional, Strict] "page" T.Text
          :> Get '[JSON] (Response Meta)
  }
  deriving stock (Generic)
