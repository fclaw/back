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

module Scaffold.Api.Controller.Frontend.Log (controller, FrontendLogRequest) where

import Scaffold.Transport.Response
import Scaffold.Transport.Payload (Payload (..))

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import qualified Data.Text as T
import GHC.Generics
import Data.Functor (($>))
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import Data.List (stripPrefix)
import Data.Typeable (typeRep)
import Data.Char (toLower)

data FrontendLogRequest = FrontendLogRequest { build :: T.Text, payload :: Payload }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor FrontendLogRequest)]] 
     FrontendLogRequest

deriveToSchemaFieldLabelModifier ''FrontendLogRequest [| 
  \s -> let (head:tail) = show (typeRep (Proxy @FrontendLogRequest))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]

controller :: FrontendLogRequest -> KatipControllerM (Response ())
controller (FrontendLogRequest _ msg) = $(logTM) InfoS (logStr (show msg)) $> Ok ()


