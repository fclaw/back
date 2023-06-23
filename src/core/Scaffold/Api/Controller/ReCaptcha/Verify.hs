{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.ReCaptcha.Verify (Token, ReCaptcha, controller) where

import Control.Lens
import Control.Lens.Iso.Extended (bytesLazy, textbs)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (parseJSON), ToJSON, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Generic.DerivingVia
import Data.Bifunctor (first)
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import Data.Swagger hiding (Response)
import Data.Text (Text, pack)
import Data.Traversable (for)
import Data.Typeable (typeRep)
import GHC.Exts
import GHC.Generics (Generic)
import Katip
import KatipController
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types.Method (methodPost)
import Scaffold.Transport.Response

newtype Token = Token Text
  deriving (Generic)
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Token

-- {
--   "success": true|false,
--   "challenge_ts": timestamp,  // timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
--   "hostname": string,         // the hostname of the site where the reCAPTCHA was solved
--   "error-codes": [...]        // optional
-- }
data ReCaptcha = ReCaptcha
  { success :: !Bool,
    errors :: !(Maybe [Text])
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON)
    via WithOptions
          '[OmitNothingFields 'True]
          ReCaptcha

instance FromJSON ReCaptcha where
  parseJSON =
    withObject "ReCaptchaResp" $ \o -> do
      success <- o .: "success"
      errors <- o .:? "error-codes"
      pure $ ReCaptcha {..}

instance ToSchema ReCaptcha where
  declareNamedSchema _ = do
    success <- declareSchemaRef (Proxy @Bool)
    errors <- declareSchemaRef (Proxy @(Maybe [Text]))
    let ident = pack $ show (typeRep (Proxy @ReCaptcha))
    pure $
      NamedSchema (Just ident) $
        mempty
          & type_ ?~ SwaggerObject
          & properties
            .~ fromList
              [("success", success)]

controller :: Token -> KatipControllerM (Response ReCaptcha)
controller token = do
  keym <- fmap (^. katipEnv . captchaKey) ask
  resp <- for keym $ \key -> do
    liftIO $ do
      manager <- Http.newManager tlsManagerSettings
      initReq <- Http.parseRequest "https://www.google.com/recaptcha/api/siteverify"
      let req =
            initReq
              { Http.method = methodPost,
                Http.queryString =
                  "secret="
                    <> key ^. textbs
                    <> "&response="
                    <> (coerce token) ^. textbs,
                -- headers: { "Content-Type": "application/x-www-form-urlencoded" }
                Http.requestHeaders =
                  [(hContentType, "application/x-www-form-urlencoded; charset=utf-8")]
              }
      resp <- fmap Http.responseBody $ Http.httpLbs req manager
      let toEither x@ReCaptcha {success, errors = Just []} = Right x
          toEither x@ReCaptcha {success, errors = Nothing} = Right x
          toEither ReCaptcha {errors = Just xs} = Left xs
      pure $ first (flip (:) [] . pack) (eitherDecodeStrict @ReCaptcha (resp ^. bytesLazy)) >>= toEither
  $(logTM) DebugS $ logStr $ "captcha resp --> " <> show resp
  when (isNothing resp) $ $(logTM) ErrorS "captcha key hasn't been found. skip"
  return $ maybe (Error (asError @Text "captcha verification cannot be fetched")) fromEithers resp
