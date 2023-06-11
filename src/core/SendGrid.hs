{-# LANGUAGE OverloadedStrings #-}

module SendGrid (configure) where 

import OpenAPI.Configuration
import OpenAPI.Common (Configuration (..))
import qualified Data.Text as T
import OpenAPI.SecuritySchemes

type BaseUrl = T.Text
type ApiKey = T.Text

configure :: BaseUrl -> ApiKey -> Configuration
configure url key = 
  defaultConfiguration 
  { configBaseURL = url
  , configSecurityScheme = 
    apiKeyInHeaderAuthenticationSecurityScheme ("Bearer " <> key) 
  }
