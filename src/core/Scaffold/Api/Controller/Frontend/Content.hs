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
{-# LANGUAGE RecordWildCards #-}

module Scaffold.Api.Controller.Frontend.Content (controller, Content) where

import Scaffold.Transport.Response

import OpenAPI.Operations.Repos_get_content
import OpenAPI.Types.ContentFile
import "github" OpenAPI.Common

import Katip
import KatipController hiding (Service)
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import Data.Swagger hiding (Response)
import GHC.Generics hiding (from, to)
import Control.Lens
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)
import BuildInfo (location)
import Data.Default.Class
import Data.Text.Extended ()
import Data.Traversable (for)
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (when)
import Control.Monad.IO.Class
import Control.Concurrent.Async.Lifted (forConcurrently)
import Network.HTTP.Client (responseStatus, responseBody)
import Network.HTTP.Types.Status (ok200, accepted202)
import Data.Functor (($>))
import Data.ByteString.Base64 (decodeLenient)
import Control.Lens.Iso.Extended (textbs)

newtype Home = Home T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)

instance Default Home
instance ToSchema Home

newtype About = About T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)
  
instance Default About
instance ToSchema About

newtype Service = Service T.Text
  deriving stock Generic
  deriving newtype (ToJSON, FromJSON)
  
instance Default Service
instance ToSchema Service

data Content = 
     Content 
     { home :: Home
     , about :: About
     , service :: Service }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Content)]] 
     Content

instance Default Content

instance ToSchema Content where
  declareNamedSchema _ = do
    home <- declareSchemaRef (Proxy @Home)
    about <- declareSchemaRef (Proxy @About)
    service <- declareSchemaRef (Proxy @Service)
    pure $ NamedSchema (Just ($location <> "." <> (show (typeRep @Content))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ 
           fromList [ 
            ("home", home)
          , ("about", about)
          , ("service", service) ]

controller :: KatipControllerM (Response Content)
controller = do 
  cfg <- fmap (^.katipEnv.github) ask
  resp <- for cfg $ \github -> do
    resp <- forConcurrently reqXs (liftIO . runWithConfiguration github . repos_get_content)
    let res = sequence $ map handleResp resp
    case res of
      Right [homeCnt, aboutCnt, serviceCnt] -> 
        return $ 
          Ok def { 
            home = Home homeCnt
          , about = About aboutCnt
          , service = Service serviceCnt }
      Left err ->
        $(logTM) ErrorS (logStr ("Github error: " <> err))
        $> Error (asError @T.Text "something went wrong")
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ fromMaybe (Ok def) resp

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

reqXs = map (flip (mkRepos_get_contentParameters "fclaw") "turkish-trade-house-docs") ["home.txt", "about.txt", "services.txt"]