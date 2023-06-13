{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module SendGrid (configure) where 

import "sendgrid" OpenAPI.Configuration
import "sendgrid" OpenAPI.Common (Configuration (..))
import qualified Data.Text as T
import "sendgrid" OpenAPI.SecuritySchemes

type BaseUrl = T.Text
type ApiKey = T.Text

configure :: BaseUrl -> ApiKey -> Configuration
configure url key = 
  defaultConfiguration 
  { configBaseURL = url
  , configSecurityScheme = 
    apiKeyInHeaderAuthenticationSecurityScheme ("Bearer " <> key) 
  }
