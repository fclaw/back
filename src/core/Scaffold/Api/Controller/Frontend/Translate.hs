{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}


module Scaffold.Api.Controller.Frontend.Translate 
   (controller, handleResp, Lang (..), Location (..), Resource (..), Translation, MenuItem) 
   where


import Scaffold.Transport.Response
import Scaffold.EnvKeys (repo)

import OpenAPI.Operations.Repos_get_content
import "github" OpenAPI.Common
import OpenAPI.Types.ContentFile

import Katip
import TH.Mk
import KatipController
import qualified Data.Text as T
import GHC.Generics (Generic)
import Control.Lens
import Control.Lens.Iso.Extended (jsonb, stext)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as Map
import Data.Traversable (for)
import Data.Maybe (isNothing)
import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Network.HTTP.Client as HTTP
import Network.HTTP.Client (responseStatus, responseBody)
import Network.HTTP.Types.Status (ok200, accepted202)
import Data.ByteString.Base64 (decodeLenient)
import Control.Lens.Iso.Extended (textbs)
import Data.Default.Class
import Data.Aeson.Generic.DerivingVia
import Data.Yaml (decodeEither', prettyPrintParseException)
import Data.Bifunctor (first)
import Data.Swagger hiding (Response)
import BuildInfo (location)
import GHC.Exts
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)

data Lang = English | Turkish
  deriving stock Generic
  deriving Enum

instance Default Lang where 
  def = English

data Location = Home | About | Service
  deriving stock Generic
  deriving Enum

instance Default Location where
  def = Home

data Resource = Content | Menu
  deriving stock Generic
  deriving Enum

instance Show Resource where
  show Content = "content"
  show Menu = "menu"

instance Default Resource where
  def = Content

data MenuItem = MenuItemHome | MenuItemAbout | MenuItemService
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ SumEnc ObjWithSingleField
     ,  ConstructorTagModifier 
        '[ UserDefined ToLower
        ,  UserDefined (StripConstructor MenuItem)] ] 
     MenuItem

 
data MenuItemObj = MenuItemObj { menuItemObjKey :: !T.Text, menuItemObjValue :: !T.Text }  
  deriving stock Generic
  deriving (FromJSON, ToJSON)
     via WithOptions 
     '[ FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor MenuItemObj)]] 
     MenuItemObj

instance ToSchema MenuItemObj where
  declareNamedSchema _ = do
    text <- declareSchemaRef (Proxy @T.Text)
    pure $ NamedSchema (Just ($location <> "." <> (show (typeRep @MenuItemObj))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ 
           fromList [ ("key", text), ("value", text) ]

data Translation = 
       TranslationContent T.Text 
     | TranslationMenu [MenuItemObj]
  deriving stock Generic
  deriving (ToJSON)
     via WithOptions 
     '[ SumEnc ObjWithSingleField ] 
     Translation

instance ToSchema Translation 


mkToSchemaAndJSON ''Lang
mkToSchemaAndJSON ''Location
mkToSchemaAndJSON ''Resource
mkEnumConvertor ''Lang
mkEnumConvertor ''Location
mkEnumConvertor ''Resource
mkParamSchemaEnum ''Lang [| isoLang.jsonb |]
mkParamSchemaEnum ''Location [| isoLocation.jsonb |]
mkParamSchemaEnum ''Resource [| isoResource.jsonb |]
mkFromHttpApiDataEnum ''Lang [| from stext.from isoLang.to Right |]
mkFromHttpApiDataEnum ''Location [| from stext.from isoLocation.to Right |]
mkFromHttpApiDataEnum ''Resource [| from stext.from isoResource.to Right |]

controller :: Resource -> Lang -> Maybe Location -> KatipControllerM (Response Translation)
controller Content lang (Just location) = do
  cfg <- fmap (^.katipEnv.github) ask
  let getTranslation repoXs = do 
        let docs_repo = repoXs Map.! "frontDocs"
        let resource = (location^.isoLocation <> "-" <> lang^.isoLang <> ".txt")^.stext
        let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
        fmap handleResp $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) (fromEither . fmap TranslationContent) resp
controller Content lang Nothing = pure $ Error $ asError @T.Text "Content requires params location to be set"
controller value@Menu lang _ = do
  cfg <- fmap (^.katipEnv.github) ask
  let getTranslation repoXs = do 
        let docs_repo = repoXs Map.! "frontDocs"
        let resource = (value^.isoResource <> "-" <> lang^.isoLang <> ".yaml")^.stext
        let req = mkRepos_get_contentParameters "fclaw" resource (repo (snd docs_repo))
        fmap handleRespMenu $ liftIO $ runWithConfiguration (fst docs_repo) $ repos_get_content req
  resp <- for cfg getTranslation
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ maybe (Error (asError @T.Text "translation cannot be fetched")) (fromEither . fmap TranslationMenu) resp

handleResp :: HTTP.Response Repos_get_contentResponse -> Either T.Text T.Text
handleResp resp =
    if responseStatus resp == ok200 || 
      responseStatus resp == accepted202 
    then let mkResp 
               (Repos_get_contentResponse200 (
                 Repos_get_contentResponseBody200Content_file (
                   Content_file {..}))) 
               = Right $ content_fileContent^.textbs.to decodeLenient.from textbs
             mkResp (Repos_get_contentResponse200 _) 
               = Left "there is no file: directory, symlink, submodule"
             mkResp err = Left $ (show err)^.stext
         in  mkResp $ responseBody resp
    else Left $ show (responseBody resp)^.stext

handleRespMenu :: HTTP.Response Repos_get_contentResponse -> Either T.Text [MenuItemObj]
handleRespMenu resp = 
  if responseStatus resp == ok200 || 
      responseStatus resp == accepted202 
  then let mkMap 
            (Repos_get_contentResponse200 (
              Repos_get_contentResponseBody200Content_file (
                Content_file {..})))
            =  first (T.pack . prettyPrintParseException) $ decodeEither' $ content_fileContent^.textbs.to decodeLenient        
           mkMap (Repos_get_contentResponse200 _) 
             = Left "there is no file: directory, symlink, submodule"
           mkMap err = Left $ (show err)^.stext
       in  mkMap $ responseBody resp   
  else Left $ show (responseBody resp)^.stext