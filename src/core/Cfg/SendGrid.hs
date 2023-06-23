{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cfg.SendGrid (configure) where

import qualified Data.Text as T
import "sendgrid" OpenAPI.Common (Configuration (..))
import "sendgrid" OpenAPI.Configuration
import "sendgrid" OpenAPI.SecuritySchemes

type BaseUrl = T.Text

type ApiKey = T.Text

configure :: BaseUrl -> ApiKey -> Configuration
configure url key =
  defaultConfiguration
    { configBaseURL = url,
      configSecurityScheme =
        apiKeyInHeaderAuthenticationSecurityScheme ("Bearer " <> key)
    }
