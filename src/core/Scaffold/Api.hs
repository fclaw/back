{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api
       ( Api (..)
       , HttpApi (..)
       , FileApi (..)
       , AdminApi (..)
       , AuthApi (..)
       , FrontendApi (..)
       , UserApi (..)
       , PublicApi (..)
       , SendGridApi (..)
       , ForeignApi (..)
       , api
       , swaggerHttpApi
       ) where

import Scaffold.Api.Map

import BuildInfo
import Servant.API
import Servant.API.Generic
import Data.Proxy
import Servant.Swagger
import Data.Swagger
import Control.Lens
import Control.Lens.Iso.Extended
import Servant.Swagger.RawM ()
import Servant.Auth.Swagger ()

newtype Api route = Api { _apiHttp :: route :- ToServant HttpWrapperApi AsApi } deriving stock Generic

newtype HttpWrapperApi route =
        HttpWrapperApi
        { _httpWrapperApiApi
          :: route
          :- Description "http api"
          :> "api"
          :> ToServant HttpApi AsApi
        } deriving stock Generic

api :: Proxy (ToServantApi Api)
api = genericApi (Proxy :: Proxy Api)

swaggerHttpApi :: String -> Maybe Int -> Version -> Swagger
swaggerHttpApi url port ver =
  toSwagger (genericApi (Proxy @HttpWrapperApi))
  & schemes ?~ [Http, Https]
  & host ?~ Host url (fmap fromIntegral port)
  & info.description ?~ "Scaffold server api"^.stext
  & info.version .~ show ver^.stext
  & info.contact ?~ Contact Nothing Nothing (Just ("fclaw007@gmail.com"^.stext))
  & info.title .~ "Scaffold. Tag (" <> $gitTag <> "). Commit (" <> $gitCommit <> ")"