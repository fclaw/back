{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}

module Scaffold.Api.Controller.Controller (controller) where

import Scaffold.Api
-- controllers
import qualified Scaffold.Api.Controller.File.Upload as File.Upload
import qualified Scaffold.Api.Controller.File.Download as File.Download
import qualified Scaffold.Api.Controller.File.Delete as File.Delete
import qualified Scaffold.Api.Controller.File.Patch as File.Patch
import qualified Scaffold.Api.Controller.Frontend.Log as Frontend.Log
import qualified Scaffold.Api.Controller.SendGrid.SendMail as SendGrid.Send 
import Servant.RawM.Server ()
import Scaffold.Auth
import Scaffold.Transport.Response
import Scaffold.Transport.Model.User (BasicAuth (..))

import Data.Aeson
import Katip
import KatipController
import Servant.Server.Generic
import Servant.API.Generic
import Servant.Ip
import Control.Monad.Time
import BuildInfo
import Data.Functor
import Servant.Auth.Server (AuthResult (..), wwwAuthenticatedErr)
import Control.Monad.Except
import qualified Network.WebSockets.Connection as WS
import qualified Network.WebSockets as WS
import System.Info
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Version (showVersion)


controller :: Api (AsServerT KatipController)
controller = Api { _apiHttp = toServant . httpApi  }

httpApi :: Maybe IP4 -> HttpApi (AsServerT KatipController)
httpApi _ =
  HttpApi 
  { _httpApiFile = toServant file
  , _httpApiAdmin = (`withBasicAuth` toServant . admin)
  , _httpApiAuth = toServant auth
  , _httpApiFront = toServant frontend
  , _httpApiUser =
      \case
        Authenticated u -> toServant $ user u
        _ -> 
          const $ 
            throwError $ 
              wwwAuthenticatedErr 
              "only for authorized personnel" 
  , _httpApiPublic = toServant public
  , _httpApiSendGrid = toServant sendgrid   
  }

file :: FileApi (AsServerT KatipController)
file =
  FileApi
  { _fileApiUpload = \bucket files ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "upload"])
    (File.Upload.controller bucket files)
  , _fileApiPatch = \fid file ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["file", "patch"])
    (File.Patch.controller fid file)
  , _fileApiDelete =
      flip logExceptionM ErrorS
    . katipAddNamespace
     (Namespace ["file", "delete"])
    . File.Delete.controller
  , _fileApiDownload = \option fid w h ->
     flip logExceptionM ErrorS $
     katipAddNamespace
     (Namespace ["file", "download"])
     (File.Download.controller option fid w h) }

admin :: User -> AdminApi (AsServerT KatipController)
admin _ = 
  AdminApi {
    _adminApiTest = do
      ct <- currentTime
      runTelegram $location $ show [1, 2]
      runTelegram $location (show ct) $> Ok ct }

auth :: AuthApi (AsServerT KatipController) 
auth = 
  AuthApi 
  { _authApiAuthWithBasic = \_ -> 
    flip logExceptionM ErrorS $
     katipAddNamespace
    (Namespace ["auth", "login", "basic"]) 
    (return $ Ok $ BasicAuth "ZmNsYXcwMDdAZ21haWwuY29tOnRlc3Q=") }

frontend :: FrontendApi (AsServerT KatipController)
frontend = 
  FrontendApi 
  { _frontendApiLog = \req -> 
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["frontend", "log"]) 
    (Frontend.Log.controller req) }

user :: User -> UserApi (AsServerT KatipController) 
user _ =
  UserApi 
  { _userApiGetProfile = \_ ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["user", "profile", "get"])
    undefined }

public :: PublicApi (AsServerT KatipController)
public = 
  PublicApi 
  { _publicApiGetServerInfo = 
     \(conn :: WS.PendingConnection) ->
    flip logExceptionM ErrorS $
    katipAddNamespace
    (Namespace ["public", "server", "info"])
    (liftIO $ do
      c <- WS.acceptRequest conn
      WS.pingThread c 1 $ do 
        let serverInfo = 
              "os: " <> 
              os <> 
              ", arch:" <> 
              arch <> 
              ", Haskell compiler: " <> 
              showVersion compilerVersion
        st <- getSystemTime
        let msg = serverInfo <> ", server time: "  <> show (systemSeconds st)
        let resp = Ok [msg]
        WS.sendDataMessage c (WS.Text (Data.Aeson.encode resp) Nothing) ) }

sendgrid :: SendGridApi  (AsServerT KatipController)
sendgrid = 
  SendGridApi
  { _sendGridApiSendMail =
    flip logExceptionM ErrorS
    . katipAddNamespace
    (Namespace ["sendgrid", "send"])
    . SendGrid.Send.controller }