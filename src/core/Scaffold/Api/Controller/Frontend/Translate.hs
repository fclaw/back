{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.Frontend.Translate (controller, handleResp, handleRespYaml, Lang (..), Location (..), Resource (..), Translation, MenuItem) where

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
import Data.Swagger hiding (Response)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Traversable (for)
import Data.Typeable (typeRep)
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
import Scaffold.EnvKeys (repo)
import Scaffold.Transport.Response
import TH.Mk

data Lang = English | Turkish
  deriving stock (Generic)
  deriving (Enum)

instance Default Lang where
  def = English

data Location = Home | About | Service
  deriving stock (Generic)
  deriving (Enum)

instance Default Location where
  def = Home

data Resource = Content | Menu
  deriving stock (Generic)
  deriving (Enum)

instance Show Resource where
  show Content = "content"
  show Menu = "menu"

instance Default Resource where
  def = Content

data MenuItem = MenuItemHome | MenuItemAbout | MenuItemService
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[ SumEnc ObjWithSingleField,
             ConstructorTagModifier
               '[ UserDefined ToLower,
                  UserDefined (StripConstructor MenuItem)
                ]
           ]
          MenuItem

data MenuItemObj = MenuItemObj {menuItemObjKey :: !T.Text, menuItemObjValue :: !T.Text}
  deriving stock (Generic)
  deriving
    (FromJSON, ToJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor MenuItemObj)]]
          MenuItemObj

deriveToSchemaFieldLabelModifier
  ''MenuItemObj
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @MenuItemObj))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

data Translation
  = TranslationContent T.Text
  | TranslationMenu [MenuItemObj]
  deriving stock (Generic)
  deriving
    (ToJSON)
    via WithOptions
          '[SumEnc ObjWithSingleField]
          Translation

instance ToSchema Translation

mkToSchemaAndJSON ''Lang
mkToSchemaAndJSON ''Location
mkToSchemaAndJSON ''Resource
mkEnumConvertor ''Lang
mkEnumConvertor ''Location
mkEnumConvertor ''Resource
mkParamSchemaEnum ''Lang [|isoLang . jsonb|]
mkParamSchemaEnum ''Location [|isoLocation . jsonb|]
mkParamSchemaEnum ''Resource [|isoResource . jsonb|]
mkFromHttpApiDataEnum ''Lang [|from stext . from isoLang . to Right|]
mkFromHttpApiDataEnum ''Location [|from stext . from isoLocation . to Right|]
mkFromHttpApiDataEnum ''Resource [|from stext . from isoResource . to Right|]

controller :: Resource -> Lang -> Maybe Location -> KatipControllerM (Response Translation)
controller Content lang (Just location) = do
  cfg <- fmap (^. katipEnv . github) ask
  let getTranslation repoXs = do
        let docs_repo = repoXs Map.! "frontDocs"
        let resource = (location ^. isoLocation <> "-" <> lang ^. isoLang <> ".txt") ^. stext
        let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
        fmap handleResp $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) (fromEither . fmap TranslationContent) resp
controller Content lang Nothing = pure $ Error $ asError @T.Text "Content requires params location to be set"
controller value@Menu lang _ = do
  cfg <- fmap (^. katipEnv . github) ask
  let getTranslation repoXs = do
        let docs_repo = repoXs Map.! "frontDocs"
        let resource = (value ^. isoResource <> "-" <> lang ^. isoLang <> ".yaml") ^. stext
        let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
        fmap (handleRespYaml @[MenuItemObj]) $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) (fromEither . fmap TranslationMenu) resp

handleResp :: HTTP.Response Repos_get_contentResponse -> Either T.Text T.Text
handleResp resp =
  if responseStatus resp == ok200
    || responseStatus resp == accepted202
    then
      let mkResp
            ( Repos_get_contentResponse200
                ( Repos_get_contentResponseBody200Content_file
                    (Content_file {..})
                  )
              ) =
              Right $ content_fileContent ^. textbs . to decodeLenient . from textbs
          mkResp (Repos_get_contentResponse200 _) =
            Left "there is no file: directory, symlink, submodule"
          mkResp err = Left $ (show err) ^. stext
       in mkResp $ responseBody resp
    else Left $ show (responseBody resp) ^. stext

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
