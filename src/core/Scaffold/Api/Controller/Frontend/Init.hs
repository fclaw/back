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

module Scaffold.Api.Controller.Frontend.Init (controller, Init) where

import Scaffold.Transport.Response
import Scaffold.EnvKeys (repo, resources)
import Scaffold.Api.Controller.Frontend.Translate (handleResp, Lang (..), Location)
import qualified Scaffold.Api.Controller.Frontend.Translate as Tr
import Scaffold.Api.Controller.Frontend.GetCookies (cookieTitle)

import OpenAPI.Operations.Repos_get_content
import OpenAPI.Operations.Git_get_ref
import "github" OpenAPI.Common
import OpenAPI.Types.GitRef 

import Katip
import KatipController hiding (Service)
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import qualified Data.Text as T
import Data.Swagger hiding (Response)
import GHC.Generics hiding (from, to)
import Control.Lens
import Data.Proxy (Proxy (..))
import Control.Lens.Iso.Extended (stext)
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
import qualified Data.Map as Map
import Scaffold.Transport.Id
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import Data.List (stripPrefix)
import Data.Typeable (typeRep)
import Data.Char (toLower)


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

deriveToSchemaFieldLabelModifier ''Content [| 
  \s -> let (head:tail) = show (typeRep (Proxy @Content))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]

data Init =
     Init 
     { content :: !Content
     , shaCommit :: !T.Text
     , shaCommitCss :: !T.Text 
     , lang :: ![Lang]
     , page :: ![Location]
     , cookies :: ![T.Text]
     }
  deriving stock Generic
  deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[ UserDefined (StripConstructor Init)]] 
     Init

instance Default Init

deriveToSchemaFieldLabelModifier ''Init [| 
  \s -> let (head:tail) = show (typeRep (Proxy @Init))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]


defInit = Init def def def def def def

controller :: Maybe (Id "browserIdent") -> KatipControllerM (Response Init)
controller _ = do 
  cfg <- fmap (^.katipEnv.github) ask
  resp <- for cfg $ \repoXs -> do
    -- frontDocs
    let docs_repo = repoXs Map.! "frontDocs"
    resp_frontDocs <- 
      forConcurrently 
      (reqXs (repo (snd docs_repo)) (resources (snd docs_repo))) 
      (liftIO . runWithConfiguration (fst docs_repo) . repos_get_content)
    let res_docs_repo = sequence $ map handleResp resp_frontDocs
    
    -- front
    let front_repo = repoXs Map.! "front"
    resp_front <- fmap handleRespFront $ liftIO $ runWithConfiguration (fst front_repo) $ git_get_ref (mkGitRef "master" (repo (snd front_repo)))
    
    -- css
    let css_repo = repoXs Map.! "frontCSS"
    resp_css <- fmap handleRespFront $ liftIO $ runWithConfiguration (fst css_repo) $ git_get_ref (mkGitRef "main" (repo (snd css_repo)))

    let mkInit = do
          [homeCnt, aboutCnt, serviceCnt] <- res_docs_repo
          shaCommit <- resp_front
          shaCommitCss <- resp_css
          pure $ 
            Init (def { 
                home = Home homeCnt
              , about = About aboutCnt
              , service = Service serviceCnt })
              shaCommit
              shaCommitCss
              [English .. Turkish]
              [Tr.Home .. Tr.Service]
              [cookieTitle]

    case mkInit of
      Right init -> return $ Ok init
      Left err ->
        $(logTM) ErrorS (logStr ("Github error: " <> err))
        $> Error (asError @T.Text "something went wrong")
  when (isNothing resp) $ $(logTM) InfoS "github key hasn't been found. skip"
  return $ fromMaybe (Ok defInit) resp

handleRespFront resp = 
    if responseStatus resp == ok200 || 
       responseStatus resp == accepted202
    then let mkResp 
               (Git_get_refResponse200 (
                 Git_ref {git_refObject = Git_refObjectgithub {..}})) 
               = Right git_refObjectgithubSha
         in  mkResp $ responseBody resp
    else Left $ show (responseBody resp)^.stext

reqXs repo = map (flip (mkRepos_get_contentParameters "fclaw") repo)
mkGitRef branch = mkGit_get_refParameters "fclaw" ("heads/" <> branch)