{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module Scaffold.Api.User (AuthApi (..), UserApi (..)) where

import Servant.API.Generic ( Generic, GenericMode(type (:-)) )
import Servant.API.Extended ( Post, type (:>), JSON, ReqBody )
import Scaffold.Transport.Response ( Response )
import Scaffold.Transport.Model.User

newtype AuthApi route =
        AuthApi {
          _authApiAuthWithBasic
          :: route
          :- "login"
          :> "basic"
          :> ReqBody '[JSON] BasicCredentials
          :> Post '[JSON] (Response BasicAuth)
        } deriving stock Generic


newtype UserApi route =
        UserApi {
          _userApiGetProfile
          :: route
          :- "profile"
          :> ReqBody '[JSON] BasicCredentials
          :> Post '[JSON] (Response ())
        } deriving stock Generic