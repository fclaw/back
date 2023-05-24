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

module Scaffold.Controller.Frontend.Log (controller, Request) where

import Scaffold.Transport.Response
import Scaffold.Config (FrontendBuild (..))

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import Data.List (stripPrefix)
import Data.Typeable
import Data.Reflection (Reifies (..))
import Data.Maybe
import qualified Data.Text as T
import Data.Swagger hiding (Response)
import GHC.Generics
import Control.Lens
import Data.Functor (($>))

data Request = Request { build :: T.Text, msg :: T.Text }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Request)]] 
     Request

instance Typeable a => Reifies (StripConstructor a) (String -> String) where
  reflect _ = \s -> fromMaybe s $ stripPrefix (show (typeRep (Proxy @a))) s

instance ToSchema Request where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @T.Text)
    pure $ NamedSchema (Just "Request(Frontend.Log)") $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList [ ("build", textSchema), ("msg", textSchema) ]

controller :: Request -> KatipController (Response ())
controller (Request in_build msg) = do
  build <- fmap (^.katipEnv.frontendBuild) ask
  if coerce build == in_build then
    $(logTM) InfoS (logStr msg) $> Ok ()
  else pure $ Error $ asError @T.Text  "credentials mismatched"


