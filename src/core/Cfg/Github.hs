{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cfg.Github (configure) where

import qualified Data.Text as T
import "github" OpenAPI.Common (Configuration (..))
import "github" OpenAPI.Configuration
import "sendgrid" OpenAPI.SecuritySchemes

type ApiKey = T.Text

configure :: ApiKey -> Configuration
configure key =
  defaultConfiguration
    { configSecurityScheme =
        apiKeyInHeaderAuthenticationSecurityScheme ("Bearer " <> key)
    }
