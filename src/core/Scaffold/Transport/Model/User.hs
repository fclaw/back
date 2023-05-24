{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Scaffold.Transport.Model.User (BasicAuth (..), BasicCredentials (..)) where

import Data.Swagger
import GHC.Generics
import Data.Proxy
import Data.Text (Text)
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Control.Lens
import GHC.Exts
import Data.List (stripPrefix)
import Data.Typeable
import Data.Reflection (Reifies (..))
import Data.Maybe


newtype BasicAuth = BasicAuth Text
  deriving stock Generic
  deriving anyclass ToParamSchema
  deriving newtype (ToJSON, FromJSON)

instance ToSchema BasicAuth where
  declareNamedSchema _ =
    pure $
    NamedSchema (Just "BasicAuth") $
    toSchema (Proxy @Text)

instance Typeable a => Reifies (StripConstructor a) (String -> String) where
  reflect _ = \s -> fromMaybe s $ stripPrefix (show (typeRep (Proxy @a))) s

data BasicCredentials = BasicCredentials { login :: Text, password :: Text } 
  deriving stock Generic
  deriving (ToJSON, FromJSON)
    via WithOptions 
        '[ FieldLabelModifier '[ UserDefined (StripConstructor BasicCredentials)]] 
        BasicCredentials

instance ToSchema BasicCredentials where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    pure $ NamedSchema (Just "BasicCredentials") $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList [ ("login", textSchema), ("password", textSchema) ]


