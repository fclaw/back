{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.Foreign
  ( ForeignApi (..),
    module SendGrid,
  )
where

import Scaffold.Api.Foreign.SendGrid as SendGrid
import Servant.API.Extended (AsApi, ToServant, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype ForeignApi route = ForeignApi
  { _foreignApiSendGrid ::
      route
        :- "sendgrid"
          :> ToServant SendGridApi AsApi
  }
  deriving stock (Generic)
