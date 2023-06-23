{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.User (AuthApi (..), UserApi (..)) where

import Scaffold.Transport.Model.User
import Scaffold.Transport.Response (Response)
import Servant.API.Extended (JSON, Post, ReqBody, type (:>))
import Servant.API.Generic (Generic, GenericMode (type (:-)))

newtype AuthApi route = AuthApi
  { _authApiAuthWithBasic ::
      route
        :- "login"
          :> "basic"
          :> ReqBody '[JSON] BasicCredentials
          :> Post '[JSON] (Response BasicAuth)
  }
  deriving stock (Generic)

newtype UserApi route = UserApi
  { _userApiGetProfile ::
      route
        :- "profile"
          :> ReqBody '[JSON] BasicCredentials
          :> Post '[JSON] (Response ())
  }
  deriving stock (Generic)
