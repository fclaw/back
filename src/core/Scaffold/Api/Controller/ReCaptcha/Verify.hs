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


module Scaffold.Api.Controller.ReCaptcha.Verify (Token, controller) where

import Scaffold.Transport.Response

import Katip
import KatipController
import Data.Text (Text, pack)
import Data.Aeson (ToJSON, FromJSON (parseJSON), encode, eitherDecodeStrict, withObject, (.:))
import Data.Swagger (ToSchema)
import GHC.Generics (Generic)
import Control.Lens ((^.))
import Data.Text.Internal (showText)
import GHC.Prim (coerce)
import qualified Network.HTTP.Client as Http
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Control.Monad.IO.Class (liftIO)
import Network.HTTP.Types.Method (methodPost)
import Data.Aeson.Generic.DerivingVia
import Data.Traversable (for)
import Control.Monad (when)
import Data.Maybe (isNothing)
import Control.Lens.Iso.Extended (bytesLazy)
import Data.Bifunctor (bimap)

newtype Token = Token Text
  deriving Generic
  deriving newtype (ToJSON, FromJSON)

instance ToSchema Token

data Body = Body { bodySecret :: Text, bodyResponse :: Text }
    deriving stock Generic
    deriving (ToJSON)
       via WithOptions 
       '[ FieldLabelModifier '[ UserDefined ToLower, UserDefined (StripConstructor Body)]] 
       Body

-- {
--   "success": true|false,
--   "challenge_ts": timestamp,  // timestamp of the challenge load (ISO format yyyy-MM-dd'T'HH:mm:ssZZ)
--   "hostname": string,         // the hostname of the site where the reCAPTCHA was solved
--   "error-codes": [...]        // optional
-- }
newtype ReCaptchaResp = ReCaptchaResp Bool
  
instance FromJSON ReCaptchaResp where
  parseJSON = withObject "ReCaptchaResp" $ \o -> fmap ReCaptchaResp $ o .: "success"

controller :: Token -> KatipControllerM (Response Bool)
controller token = do
  $(logTM) InfoS $ logStr (showText (coerce token))
  keym <- fmap (^.katipEnv.captchaKey) ask
  resp <- for keym $ \key -> do  
    liftIO $ do 
      manager <- Http.newManager tlsManagerSettings
      initReq <- Http.parseRequest "https://www.google.com/recaptcha/api/siteverify"
      let body = encode $ Body key $ coerce token
      let req = 
            initReq { 
              Http.method = 
              methodPost
            , Http.requestBody = 
              Http.RequestBodyLBS body }
      resp <- fmap Http.responseBody $ Http.httpLbs req manager
      pure $ bimap pack (coerce @ReCaptchaResp) $ eitherDecodeStrict $ resp^.bytesLazy
  when (isNothing resp) $ $(logTM) ErrorS "captcha key hasn't been found. skip"
  return $ maybe (Error (asError @Text "captcha verification cannot be fetched")) fromEither resp  
