{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.ReCaptcha (ReCaptchaApi (..)) where

import Scaffold.Api.Controller.ReCaptcha.Verify (Token)
import Scaffold.Transport.Response ( Response )

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended (type (:>), type (:>), ReqBody, Post, JSON )
import Servant.Swagger.Internal.Extended ()

newtype ReCaptchaApi route =
        ReCaptchaApi {
          _reCaptchaApiVerify
          :: route
          :- "verify"
          :> ReqBody '[JSON] Token
          :> Post '[JSON] (Response Bool)
        } deriving stock Generic