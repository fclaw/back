{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.Protected (AdminApi (..)) where

import Data.Time (UTCTime)
import Scaffold.Transport.Response (Response)
import Servant.API.Extended (Get, JSON, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype AdminApi route = AdminApi
  { _adminApiTest ::
      route
        :- "test"
          :> Get '[JSON] (Response UTCTime)
  }
  deriving stock (Generic)
