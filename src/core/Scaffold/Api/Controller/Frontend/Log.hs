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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.Frontend.Log (controller, FrontendLogRequest) where

import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.Functor (($>))
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Typeable (typeRep)
import GHC.Generics
import Katip
import KatipController
import Scaffold.Transport.Payload (Payload (..))
import Scaffold.Transport.Response

data FrontendLogRequest = FrontendLogRequest {build :: T.Text, payload :: Payload}
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor FrontendLogRequest)]]
          FrontendLogRequest

deriveToSchemaFieldLabelModifier
  ''FrontendLogRequest
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @FrontendLogRequest))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: FrontendLogRequest -> KatipControllerM (Response ())
controller (FrontendLogRequest _ msg) = $(logTM) InfoS (logStr (show msg)) $> Ok ()
