{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.Foreign
  (ForeignApi (..)
  , module SendGrid
  ) where

import Scaffold.Api.Foreign.SendGrid as SendGrid

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( type (:>), AsApi, ToServant )

newtype ForeignApi route =
        ForeignApi {
          _foreignApiSendGrid
          :: route
          :- "sendgrid"
          :> ToServant SendGridApi AsApi
        } deriving stock Generic