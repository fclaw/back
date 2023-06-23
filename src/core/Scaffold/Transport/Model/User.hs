{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Transport.Model.User (BasicAuth (..), BasicCredentials (..)) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Proxy
import Data.Swagger
import Data.Text (Text)
import GHC.Exts
import GHC.Generics

newtype BasicAuth = BasicAuth Text
  deriving stock (Generic)
  deriving anyclass (ToParamSchema)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema BasicAuth where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "BasicAuth") $
        toSchema (Proxy @Text)

data BasicCredentials = BasicCredentials {login :: Text, password :: Text}
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor BasicCredentials)]]
          BasicCredentials

instance ToSchema BasicCredentials where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $
      NamedSchema (Just "BasicCredentials") $
        mempty
          & type_ ?~ SwaggerObject
          & properties .~ fromList [("login", textSchema), ("password", textSchema)]
