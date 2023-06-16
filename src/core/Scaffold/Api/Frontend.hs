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
      :> Get '[JSON] (Response Init)
    , _frontendApiTranslate
      :: route
      :- "translate"
      :> Capture "page" Page
      :> Capture "lang" Lang
      :> Get '[JSON] (Response Translation)
    } deriving stock Generic