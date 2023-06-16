{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.Frontend.Translate (controller, Lang (..), Page (..), Translation) where


import Scaffold.Transport.Response
import Scaffold.Api.Controller.Frontend.Init (handleRespDocs)
import Scaffold.EnvKeys (repo)

import OpenAPI.Operations.Repos_get_content
import "github" OpenAPI.Common

import Katip
import TH.Mk
import KatipController
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Swagger (ToSchema)
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson (ToJSON)
import qualified Data.Map as Map
import Data.Traversable (for)
import Data.Maybe (isNothing)
import Control.Monad (when)
import Control.Monad.IO.Class

data Lang = English | Turkish
  deriving stock Generic

data Page = Home
  deriving stock Generic

newtype Translation = Translation T.Text
  deriving Generic
  deriving (ToJSON)

instance ToSchema Translation

mkToSchemaAndJSON ''Lang
mkToSchemaAndJSON ''Page
mkEnumConvertor ''Lang
mkEnumConvertor ''Page
mkParamSchemaEnum ''Lang [| isoLang.jsonb |]
mkParamSchemaEnum ''Page [| isoPage.jsonb |]
mkFromHttpApiDataEnum ''Lang [| from stext.from isoLang.to Right |]
mkFromHttpApiDataEnum ''Page [| from stext.from isoPage.to Right |]

controller :: Page -> Lang -> KatipControllerM (Response Translation)
controller page lang = do
  cfg <- fmap (^.katipEnv.github) ask
  let getTranslation repoXs = do 
        let docs_repo = repoXs Map.! "frontDocs"
        let resource = (page^.isoPage <> "-" <> lang^.isoLang <> ".txt")^.stext
        let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
        fmap handleRespDocs $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) (fromEither . fmap Translation) resp