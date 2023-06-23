{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}

module Scaffold.Api.Map
  ( HttpApi (..),
    module File,
    module Protected,
    module User,
    module Front,
    module Public,
    module Foreign,
    module ReCaptcha,
  )
where

import Scaffold.Api.File as File
import Scaffold.Api.Foreign as Foreign
import Scaffold.Api.Frontend as Front
import Scaffold.Api.Protected as Protected
import Scaffold.Api.Public as Public
import Scaffold.Api.ReCaptcha as ReCaptcha
import Scaffold.Api.User as User
import Scaffold.Auth
import Servant.API
import Servant.API.Generic
import qualified Servant.Auth.Server as SA
import Servant.Swagger.Tags

data HttpApi route = HttpApi
  { _httpApiFile ::
      route
        :- Tags "File"
          :> "file"
          :> ToServant FileApi AsApi,
    _httpApiAdmin ::
      route
        :- Tags "Admin"
          :> SA.Auth '[SA.BasicAuth] User
          :> "admin"
          :> ToServant AdminApi AsApi,
    _httpApiAuth ::
      route
        :- Tags "Auth"
          :> "auth"
          :> ToServant AuthApi AsApi,
    _httpApiFront ::
      route
        :- Tags "Front"
          :> "frontend"
          :> ToServant FrontendApi AsApi,
    _httpApiUser ::
      route
        :- Tags "User"
          :> "user"
          :> SA.Auth '[SA.BasicAuth, SA.JWT] User
          :> ToServant UserApi AsApi,
    _httpApiPublic ::
      route
        :- Tags "Public"
          :> "public"
          :> ToServant PublicApi AsApi,
    _httpApiForeign ::
      route
        :- Tags "Foreign"
          :> "foreign"
          :> ToServant ForeignApi AsApi,
    _httpApiReCaptcha ::
      route
        :- Tags "ReCaptcha"
          :> "captcha"
          :> ToServant ReCaptchaApi AsApi
  }
  deriving stock (Generic)
