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
{-# LANGUAGE PackageImports #-}

module Scaffold.Api.Controller.Frontend.GetMeta (controller, Meta) where

import Scaffold.Transport.Response
import Scaffold.Api.Controller.Frontend.Translate (handleRespYaml)
import Scaffold.EnvKeys (repo)

import OpenAPI.Operations.Repos_get_content
import "github" OpenAPI.Common

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import qualified Data.Text as T
import GHC.Generics hiding (Meta)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import Data.List (stripPrefix)
import Data.Typeable (typeRep)
import Data.Char (toLower)
import qualified Data.Map as Map
import Control.Lens
import Data.Maybe
import Data.Traversable (for)
import Control.Monad (when)
import Control.Monad.IO.Class

data Meta = Meta { description :: !T.Text, robot :: !(Maybe T.Text) }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Meta)]] 
     Meta

deriveToSchemaFieldLabelModifier ''Meta [| 
  \s -> let (head:tail) = show (typeRep (Proxy @Meta))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]

controller :: Maybe T.Text -> KatipControllerM (Response Meta)
controller page = do
  cfg <- fmap (^.katipEnv.github) ask
  resp <- for cfg $ \repoXs -> do
    let docs_repo = repoXs Map.! "frontDocs"
    let resource = fromMaybe "home" page <> "-meta.yaml"
    let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
    fmap (handleRespYaml @Meta) $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  when (isNothing resp) $ $(logTM) ErrorS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "meta cannot be fetched")) fromEither resp