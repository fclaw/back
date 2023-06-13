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

module Scaffold.Api.Controller.Frontend.Content (controller, Content) where

import Scaffold.Transport.Response

import KatipController hiding (Service)
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import Data.Swagger hiding (Response)
import GHC.Generics
import Control.Lens
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)
import BuildInfo (location)
import Data.Default.Class
import  Data.Text.Extended ()

newtype Home = Home T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)

instance Default Home
instance ToSchema Home

newtype About = About T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)
  
instance Default About
instance ToSchema About

newtype Service = Service T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)
  
instance Default Service
instance ToSchema Service

data Content = 
     Content 
     { home :: Home
     , about :: About
     , service :: Service }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Content)]] 
     Content

instance Default Content

instance ToSchema Content where
  declareNamedSchema _ = do
    home <- declareSchemaRef (Proxy @Home)
    about <- declareSchemaRef (Proxy @About)
    service <- declareSchemaRef (Proxy @Service)
    pure $ NamedSchema (Just ($location <> "." <> (show (typeRep @Content))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ 
           fromList [ 
            ("home", home)
          , ("about", about)
          , ("service", service) ]

controller :: KatipControllerM (Response Content)
controller = return $ Ok def