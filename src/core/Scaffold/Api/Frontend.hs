{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Frontend (FrontendApi (..)) where

import Scaffold.Api.Controller.Frontend.Log (Request)
import Scaffold.Api.Controller.Frontend.Content (Content)

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( Put, Get, type (:>), JSON, ReqBody )
import Scaffold.Transport.Response ( Response )

data FrontendApi route =
     FrontendApi {
       _frontendApiLog
      :: route
      :- "log"
      :> ReqBody '[JSON] Request
      :> Put '[JSON] (Response ())
    , _frontendApiContent
      :: route
      :- "content"
      :> Get '[JSON] (Response Content)
    } deriving stock Generic