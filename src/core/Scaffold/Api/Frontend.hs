{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Frontend (FrontendApi (..)) where

import Scaffold.Controller.Frontend.Log (Request)

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( Put, type (:>), JSON, ReqBody )
import Scaffold.Transport.Response ( Response )

newtype FrontendApi route =
        FrontendApi {
        _frontendApiLog
        :: route
        :- "log"
        :> ReqBody '[JSON] Request
        :> Put '[JSON] (Response ())
        } deriving stock Generic