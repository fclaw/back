{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-local-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Scaffold.Application (Cfg (..), AppMonad (..), run) where

import BuildInfo
import Control.Concurrent.Async
import Control.Concurrent.Lifted
import Control.Concurrent.STM.TChan
import Control.Exception
import Control.Lens
import Control.Lens.Iso.Extended
import Control.Monad.Base (MonadBase)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.RWS.Strict as RWS
import Control.Monad.STM
import Control.Monad.Trans.Control
import Crypto.JOSE.JWK
import Data.Aeson
import Data.Bool
import Data.Coerce
import Data.Either.Combinators
import Data.Generics.Product.Fields
import qualified Data.Map as M
import Data.String.Conv
import qualified Data.Text as T
import Katip
import KatipController
import Language.Haskell.TH.Syntax (Loc)
import qualified Network.HTTP.Types as H
import Network.HTTP.Types.Header.Extended
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as Warp
import Network.Wai.Middleware.Cors
import qualified Network.Wai.Middleware.Servant.Logger as Middleware
import Network.Wai.Parse
import Pretty
import Scaffold.Api
import qualified Scaffold.Api.Controller.Controller as Controller
import Scaffold.Auth (User, checkBasicAuth)
import qualified Scaffold.Config as Cfg
import Scaffold.Transport.Error
import qualified Scaffold.Transport.Response as Response
import Servant
import Servant.API.Generic
import Servant.Auth.Server
import Servant.Error.Formatters (formatters)
import Servant.Multipart
import Servant.Swagger.UI
import System.Directory
import System.FilePath.Posix
import TextShow

data Cfg = Cfg
  { cfgHost :: !String,
    cfgSwaggerPort :: !(Maybe Int),
    cfgServerPort :: !Int,
    cfgCors :: !Cfg.Cors,
    cfgServerError :: !Cfg.ServerError,
    cfgAdminStorage :: !(M.Map T.Text User),
    mute500 :: !(Maybe Bool)
  }

newtype AppMonad a = AppMonad {runAppMonad :: RWS.RWST KatipEnv KatipLogger KatipState IO a}
  deriving newtype (Functor)
  deriving newtype (Applicative)
  deriving newtype (Monad)
  deriving newtype (MonadIO)
  deriving newtype (MonadReader KatipEnv)
  deriving newtype (MonadState KatipState)
  deriving newtype (MonadWriter KatipLogger)
  deriving newtype (MonadRWS KatipEnv KatipLogger KatipState)
  deriving newtype (MonadBase IO)
  deriving newtype (MonadBaseControl IO)
  deriving newtype (MonadCatch)
  deriving newtype (MonadThrow)

run :: Cfg -> KatipContextT AppMonad ()
run Cfg {..} = katipAddNamespace (Namespace ["application"]) $ do
  telegram_service <- fmap (^. telegram) ask
  let runTelegram l msg = void $ fork $ liftIO $ sendMsg telegram_service l (mkPretty ("At module " <> $location) msg ^. stext)
  logger <- katipAddNamespace (Namespace ["application"]) askLoggerIO

  version_e <- liftIO getVersion
  whenLeft version_e $ \e -> throwM $ ErrorCall e
  let Right ver = version_e
  runTelegram logger $ "server version " <> show ver

  runTelegram logger $ "server run on: " <> "http://127.0.0.1:" <> show cfgServerPort

  $(logTM) DebugS $ ls $ "server run on: " <> "http://127.0.0.1:" <> showt cfgServerPort

  runTelegram logger $ "admin list: " <> show cfgAdminStorage

  configKatipEnv <- lift ask
  let initCfg = do
        configEnv <- getLogEnv
        configCtx <- getKatipContext
        configNm <- getKatipNamespace
        return $ Config {..}
  cfg <- initCfg
  let withSwagger :: Proxy a -> Proxy (a :<|> SwaggerSchemaUI "swagger" "swagger.json")
      withSwagger _ = Proxy

  (write_ch, read_ch) <- liftIO $ atomically $ do write_ch <- newTChan; read_ch <- dupTChan write_ch; return (write_ch, read_ch)

  let server =
        hoistServerWithContext
          (withSwagger api)
          (Proxy @'[JWTSettings, CookieSettings, BasicAuthData -> IO (AuthResult User)])
          (runKatipController cfg (KatipControllerState 0 write_ch))
          ( toServant Controller.controller
              :<|> swaggerSchemaUIServerT
                (swaggerHttpApi cfgHost cfgSwaggerPort ver)
          )
  excep <- katipAddNamespace (Namespace ["exception"]) askLoggerIO
  ctx_logger <- katipAddNamespace (Namespace ["context"]) askLoggerIO
  req_logger <- katipAddNamespace (Namespace ["request"]) askLoggerIO
  basic_auth <- katipAddNamespace (Namespace ["auth", "basic"]) askLoggerWithLocIO

  jwk <- liftIO $ genJWK (RSAGenParam (4096 `div` 8))

  let settings =
        Warp.defaultSettings
          & Warp.setPort cfgServerPort
          & Warp.setOnException (logUncaughtException excep runTelegram)
          & Warp.setOnExceptionResponse (\e -> mk500Response e (coerce cfgServerError) mute500)
          & Warp.setServerName ("scaffold api server, revision " <> $gitCommit)
          & Warp.setLogger (logRequest req_logger runTelegram)
  let multipartOpts =
        (defaultMultipartOptions (Proxy @Tmp))
          { generalOptions = clearMaxRequestNumFiles defaultParseRequestBodyOptions
          }
  let mkCtx = formatters :. defaultJWTSettings jwk :. defaultCookieSettings :. checkBasicAuth basic_auth cfgAdminStorage :. EmptyContext
  let runServer = serveWithContext (withSwagger api) mkCtx server
  mware_logger <- katipAddNamespace (Namespace ["middleware"]) askLoggerWithLocIO

  path <- liftIO getCurrentDirectory
  let tls_settings = (Warp.tlsSettings (path </> "tls/certificate") (path </> "tls/key")) {Warp.onInsecure = Warp.AllowInsecure}

  serverAsync <- liftIO $ async $ Warp.runTLS tls_settings settings (middleware cfgCors mware_logger runServer)
  mail_logger <- katipAddNamespace (Namespace ["mail"]) askLoggerIO
  teleram_logger <- katipAddNamespace (Namespace ["telegram"]) askLoggerIO
  telegramAsync <- liftIO $ async $ forever $ runMsgDeliver read_ch telegram_service teleram_logger
  liftIO (void (waitAnyCancel [serverAsync, telegramAsync])) `logExceptionM` ErrorS

middleware :: Cfg.Cors -> KatipLoggerLocIO -> Application -> Application
middleware cors log app = mkCors cors $ Middleware.logMw log app

logUncaughtException :: KatipLoggerIO -> (KatipLoggerIO -> String -> IO ()) -> Maybe Request -> SomeException -> IO ()
logUncaughtException log runTelegram req e =
  when (Warp.defaultShouldDisplayException e) $
    maybe
      ( do
          runTelegram log $ "before request being handled" <> show e
          log ErrorS (logStr ("before request being handled" <> show e))
      )
      ( \r -> do
          runTelegram log $ "\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e
          log ErrorS (logStr ("\"" <> toS (requestMethod r) <> " " <> toS (rawPathInfo r) <> " " <> toS (show (httpVersion r)) <> "500 - " <> show e))
      )
      req

mk500Response :: SomeException -> Bool -> Maybe Bool -> Response
mk500Response error cfgServerError mute500 =
  bool
    ( responseLBS
        status200
        [ (H.hContentType, "application/json; charset=utf-8"),
          (hAccessControlAllowOrigin, "*")
        ]
        $ encode @(Response.Response ())
        $ Response.Error (asError @T.Text (showt error))
    )
    mk500
    cfgServerError
  where
    mk500 =
      case mute500 of
        Just True ->
          responseLBS
            status500
            [ (H.hContentType, "text/plain; charset=utf-8"),
              (hAccessControlAllowOrigin, "*")
            ]
            (showt error ^. textbsl)
        _ ->
          responseLBS
            status200
            [ (H.hContentType, "text/json; charset=utf-8"),
              (hAccessControlAllowOrigin, "*")
            ]
            ( encode @(Response.Response ()) $
                Response.Error (asError @T.Text (showt error))
            )

logRequest :: KatipLoggerIO -> (KatipLoggerIO -> String -> IO ()) -> Request -> Status -> Maybe Integer -> IO ()
logRequest log runTelegram req _ _ = log InfoS (logStr (show req)) >> runTelegram log (mkPretty mempty req)

deriving instance Generic CorsResourcePolicy

mkCors :: Cfg.Cors -> Middleware
mkCors cfg_cors =
  cors $
    const $
      pure $
        simpleCorsResourcePolicy
          & field @"corsOrigins"
            .~ fmap ((,True) . map toS) (Cfg.corsOrigins cfg_cors)
          & field @"corsRequestHeaders"
            .~ [hAuthorization, hContentType, hOrigin]
          & field @"corsMethods"
            .~ simpleMethods
              <> [methodPut, methodPatch, methodDelete, methodOptions]
          & field @"corsIgnoreFailures" .~ True

askLoggerWithLocIO :: KatipContextT AppMonad (Maybe Loc -> Severity -> LogStr -> IO ())
askLoggerWithLocIO = do
  ctx <- getKatipContext
  ns <- getKatipNamespace
  logEnv <- getLogEnv
  pure $ \loc sev msg ->
    runKatipT logEnv $
      logItem ctx ns loc sev msg
