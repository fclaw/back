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

module Scaffold.Api.Controller.Frontend.Log (controller, Request) where

import Scaffold.Transport.Response

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import Data.Swagger hiding (Response)
import GHC.Generics
import Control.Lens
import Data.Functor (($>))
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)
import BuildInfo (location)

data Request = Request { build :: T.Text, msg :: T.Text }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Request)]] 
     Request

instance ToSchema Request where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @T.Text)
    pure $ NamedSchema (Just ($location <> "." <> (show (typeRep @Request))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList [ ("build", textSchema), ("msg", textSchema) ]

controller :: Request -> KatipControllerM (Response ())
controller (Request _ msg) = $(logTM) InfoS (logStr msg) $> Ok ()


