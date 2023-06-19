{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Foreign.SendGrid (SendGridApi (..)) where

import Scaffold.Api.Controller.SendGrid.SendMail (SendGridSendMailRequest)
import Scaffold.Transport.Response ( Response )

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( Post, type (:>), JSON, ReqBody )

newtype SendGridApi route =
        SendGridApi {
          _sendGridApiSendMail
          :: route
          :- "send"
          :> ReqBody '[JSON] SendGridSendMailRequest
          :> Post '[JSON] (Response ())
        } deriving stock Generic