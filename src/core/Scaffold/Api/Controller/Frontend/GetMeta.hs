{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.Frontend.GetMeta (controller, Meta) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Typeable (typeRep)
import GHC.Generics hiding (Meta)
import Katip
import KatipController
import "github" OpenAPI.Common
import OpenAPI.Operations.Repos_get_content
import Scaffold.Api.Controller.Frontend.Translate (handleRespYaml)
import Scaffold.EnvKeys (repo)
import Scaffold.Transport.Response

data Meta = Meta {description :: !T.Text, robot :: !(Maybe T.Text)}
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor Meta)]]
          Meta

deriveToSchemaFieldLabelModifier
  ''Meta
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @Meta))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: Maybe T.Text -> KatipControllerM (Response Meta)
controller page = do
  cfg <- fmap (^. katipEnv . github) ask
  resp <- for cfg $ \repoXs -> do
    let docs_repo = repoXs Map.! "frontDocs"
    let resource = fromMaybe "home" page <> "-meta.yaml"
    let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
    fmap (handleRespYaml @Meta) $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  when (isNothing resp) $ $(logTM) ErrorS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "meta cannot be fetched")) fromEither resp
