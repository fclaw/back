{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
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

module Scaffold.Api.Controller.SendGrid.SendMail (controller, Request) where

import Scaffold.Transport.Response

import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import GHC.Generics
import Data.Swagger hiding (Response)
import Control.Lens
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)

newtype Email = Email T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)
  deriving newtype ToSchema

-- https://docs.sendgrid.com/for-developers/sending-email/personalizations
data Personalization = 
     Personalization 
     { to :: ![Email]
     , subject :: !T.Text
     } 
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Personalization)]] 
     Personalization

instance ToSchema Personalization where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @T.Text)
    xsSchema <- declareSchemaRef (Proxy @[Email])
    pure $ NamedSchema (Just ((show (typeRep @Personalization))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList [ ("to", xsSchema), ("subject", textSchema) ]

data Request = 
     Request 
     { personalizations :: ![Personalization]
     , from :: !Email
     , theme :: !T.Text
     , body :: !T.Text 
     }
    deriving stock Generic
    deriving (ToJSON, FromJSON)
       via WithOptions 
       '[ FieldLabelModifier '[ UserDefined (StripConstructor Request)]] 
       Request

instance ToSchema Request where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @T.Text)
    emailSchema <- declareSchemaRef (Proxy @Email)
    xsSchema <- declareSchemaRef (Proxy @[Personalization])
    pure $ NamedSchema (Just ("SendGrid.SendMail." <> (show (typeRep @Request))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList 
           [ ("personalizations", xsSchema)
           , ("from", emailSchema)
           , ("theme", textSchema)
           , ("body", textSchema) ]

controller :: Request -> KatipController (Response ())
controller = undefined