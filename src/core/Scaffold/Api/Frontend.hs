{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Frontend (FrontendApi (..)) where

import Scaffold.Api.Controller.Frontend.Log (Request)
import Scaffold.Api.Controller.Frontend.Init (Init)
import Scaffold.Api.Controller.Frontend.Translate hiding (controller)
import Scaffold.Api.Controller.Frontend.GetCookies (Cookie)
import Scaffold.Transport.Id

import Servant.API.Generic ( Generic )
import Servant.API.Extended
import Scaffold.Transport.Response ( Response )

data FrontendApi route =
     FrontendApi {
       _frontendApiLog
      :: route
      :- "log"
      :> ReqBody '[JSON] Request
      :> Put '[JSON] (Response ())
    , _frontendApiInit
      :: route
      :- "init"
      :> QueryParam' '[Optional, Strict] "browserIdent" (Id "browserIdent")
      :> Get '[JSON] (Response Init)
    , _frontendApiTranslate
      :: route
      :- "translate" 
      :> Capture "resource" Resource
      :> Capture "lang" Lang
      :> QueryParam' '[Optional, Strict] "location" Location
      :> Get '[JSON] (Response Translation)
    , _frontendApiGetCookies
      :: route 
      :- "cookies"
      :> Get '[JSON] (Response [Cookie])
    } deriving stock Generic