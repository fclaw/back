{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Scaffold.Api.Controller.Frontend.Translate (controller, handleRespYaml, Lang (..), Translation, Map (..)) where

import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext, textbs)
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Generic.DerivingVia
import Data.Bifunctor (first)
import Data.ByteString.Base64 (decodeLenient)
import Data.Char (toLower)
import Data.Default.Class
import Data.List (stripPrefix)
import qualified Data.Map as Map
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Swagger (ToSchema (..), genericDeclareNamedSchema)
import Data.Swagger.ParamSchema (defaultSchemaOptions, fieldLabelModifier)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Typeable (Typeable, typeRep, typeRepTyCon)
import Data.Yaml (decodeEither', prettyPrintParseException)
import GHC.Generics (Generic)
import Katip
import KatipController
import Network.HTTP.Client (responseBody, responseStatus)
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Types.Status (accepted202, ok200)
import "github" OpenAPI.Common
import OpenAPI.Operations.Repos_get_content
import OpenAPI.Types.ContentFile
import qualified Scaffold.Api.Controller.Frontend.Translate.Enum as Enum
import Scaffold.EnvKeys (repo)
import Scaffold.Transport.Response
import TH.Mk

modify :: forall a. Typeable a => String -> String
modify =
  \s ->
    let (head : tail) = show (typeRep (Proxy @a))
     in maybe s (map toLower) (stripPrefix (toLower head : tail) s)

data Lang = English | Turkish
  deriving stock (Generic)
  deriving (Enum)

instance Default Lang where
  def = English

data Map k v = Map {mapKey :: k, mapValue :: v}
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructorParamType (Map k v))]]
          (Map k v)

instance (ToSchema k, ToSchema v, Typeable k, Typeable v) => ToSchema (Map k v) where
  declareNamedSchema =
    genericDeclareNamedSchema @(Map k v) $
      defaultSchemaOptions
        { fieldLabelModifier =
            \s ->
              let (head : tail) = show $ typeRepTyCon $ typeRep (Proxy @(Map k v))
               in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
        }

data Translation = Translation
  { translationPage :: [Map Enum.Page T.Text],
    translationMenu :: [Map Enum.Menu T.Text],
    translationMessenger :: [Map Enum.Messenger T.Text],
    translationCopyright :: !T.Text
  }
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Translation)]]
          Translation

deriveToSchemaFieldLabelModifier ''Translation [|modify @Translation|]

mkToSchemaAndJSON ''Lang
mkEnumConvertor ''Lang
mkParamSchemaEnum ''Lang [|isoLang . jsonb|]
mkFromHttpApiDataEnum ''Lang [|from stext . from isoLang . to Right|]

controller :: Lang -> KatipControllerM (Response Translation)
controller lang = do
  cfg <- fmap (^. katipEnv . github) ask
  let getTranslation repoXs = do
        let docs_repo = repoXs Map.! "frontDocs"
        let file = (lang ^. isoLang <> ".yaml") ^. stext
        let req = mkRepos_get_contentParameters "fclaw" file (repo (snd docs_repo))
        fmap (handleRespYaml @Translation) $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) fromEither resp

handleRespYaml :: FromJSON a => HTTP.Response Repos_get_contentResponse -> Either T.Text a
handleRespYaml resp =
  if responseStatus resp == ok200
    || responseStatus resp == accepted202
    then
      let mkMap
            ( Repos_get_contentResponse200
                ( Repos_get_contentResponseBody200Content_file
                    (Content_file {..})
                  )
              ) =
              first (T.pack . prettyPrintParseException) $ decodeEither' $ content_fileContent ^. textbs . to decodeLenient
          mkMap (Repos_get_contentResponse200 _) =
            Left "there is no file: directory, symlink, submodule"
          mkMap err = Left $ (show err) ^. stext
       in mkMap $ responseBody resp
    else Left $ show (responseBody resp) ^. stext
