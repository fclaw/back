{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Cfg.Github (configure) where 

import "github" OpenAPI.Configuration
import "github" OpenAPI.Common (Configuration (..))
import qualified Data.Text as T
import "sendgrid" OpenAPI.SecuritySchemes

type ApiKey = T.Text

configure :: ApiKey -> Configuration
configure key = 
  defaultConfiguration 
  { configSecurityScheme = 
    apiKeyInHeaderAuthenticationSecurityScheme ("Bearer " <> key) 
  }
