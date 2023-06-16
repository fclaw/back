{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}

module Scaffold.Api.Controller.Frontend.Translate (controller, Lang (..), Page (..), Translation) where


import Scaffold.Transport.Response

import TH.Mk
import KatipController
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Swagger (ToSchema)
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson (ToJSON)

data Lang = English | Turkish
  deriving stock Generic

data Page = Home
  deriving stock Generic

newtype Translation = Translation T.Text
  deriving Generic
  deriving (ToJSON)

instance ToSchema Translation

controller :: Page -> Lang -> KatipControllerM (Response Translation)
controller _ _ = return $ Ok $ Translation mempty

mkToSchemaAndJSON ''Lang
mkToSchemaAndJSON ''Page
mkEnumConvertor ''Lang
mkEnumConvertor ''Page
mkParamSchemaEnum ''Lang [| isoLang.jsonb |]
mkParamSchemaEnum ''Page [| isoPage.jsonb |]
mkFromHttpApiDataEnum ''Lang [| from stext.from isoLang.to Right |]
mkFromHttpApiDataEnum ''Page [| from stext.from isoPage.to Right |]