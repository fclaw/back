{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}


module Scaffold.Api.Controller.ReCaptcha.Verify (Token, ReCaptcha, controller) where

import Scaffold.Transport.Response

import Katip
import KatipController
import Data.Text (Text, pack)
import Data.Aeson (ToJSON, FromJSON (parseJSON), eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Control.Lens ((^.))
import GHC.Prim (coerce)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Method (methodPost)
import Data.Aeson.Generic.DerivingVia
import Data.Traversable (for)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Control.Lens.Iso.Extended (textbs, bytesLazy)
import Data.Bifunctor (first)
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import Data.List (stripPrefix)
import Data.Typeable (typeRep)
import Data.Char (toLower)
import Data.Proxy (Proxy (..))
import Network.HTTP.Types.Header (hContentType)

newtype Token = Token Text
  deriving Generic
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Token
  
-- {
--   "success": true|false,
--   "challenge_ts": timestamp,  // timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
--   "hostname": string,         // the hostname of the site where the reCAPTCHA was solved
--   "error-codes": [...]        // optional
-- }
data ReCaptcha = 
     ReCaptcha 
     { success :: !Bool
     , errors :: !(Maybe [Text])
     , host :: !Text 
     }
  deriving stock Generic
  deriving stock Show
  deriving (ToJSON)
    via WithOptions 
      '[ FieldLabelModifier '[ UserDefined ToLower, UserDefined (StripConstructor ReCaptcha)]] 
       ReCaptcha

instance FromJSON ReCaptcha where
  parseJSON = 
    withObject "ReCaptchaResp" $ \o -> do
      success <- o .: "success"
      errors <- o .:? "error-codes"
      host <- o .: "hostname"
      pure $ ReCaptcha {..}

deriveToSchemaFieldLabelModifier ''ReCaptcha [| 
  \s -> let (head:tail) = show (typeRep (Proxy @ReCaptcha))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]

controller :: Token -> KatipControllerM (Response ReCaptcha)
controller token = do
  keym <- fmap (^.katipEnv.captchaKey) ask
  resp <- for keym $ \key -> do
    liftIO $ do 
      manager <- Http.newManager tlsManagerSettings
      initReq <- Http.parseRequest "https://www.google.com/recaptcha/api/siteverify"
      let req = 
            initReq { 
              Http.method = methodPost
            , Http.queryString = 
              "secret=" <> 
              key^.textbs <> 
              "&response=" <> 
              (coerce token)^.textbs 
              -- headers: { "Content-Type": "application/x-www-form-urlencoded" }
            , Http.requestHeaders = 
              [(hContentType, "application/x-www-form-urlencoded; charset=utf-8")] }
      resp <- fmap Http.responseBody $ Http.httpLbs req manager
      let toEither ReCaptcha { errors = Just xs } = Left xs
          toEither x@ReCaptcha { success } = Right x
      pure $ first (flip (:) [] . pack) (eitherDecodeStrict @ReCaptcha (resp^.bytesLazy)) >>= toEither
  $(logTM) DebugS $ logStr $ "captcha resp --> " <> show resp     
  when (isNothing resp) $ $(logTM) ErrorS "captcha key hasn't been found. skip"
  return $ maybe (Error (asError @Text "captcha verification cannot be fetched")) fromEithers resp  
