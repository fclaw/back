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
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PackageImports #-}

module Scaffold.Api.Controller.SendGrid.SendMail (controller, Request) where

import Scaffold.Transport.Response
import Scaffold.Config (Email (..), SendGrid (..), email)

import OpenAPI.Operations.POST_mail_send 
  ( pOST_mail_send
  , mkPOST_mail_sendRequestBody
  , mkPOST_mail_sendRequestBodyContentsendgrid
  , mkPOST_mail_sendRequestBodyPersonalizationssendgrid
  , pOST_mail_sendRequestBodyPersonalizationssendgridSend_at
  , pOST_mail_sendRequestBodyPersonalizationssendgridSubject
  )
import OpenAPI.Types.FromEmailObject (mkFrom_email_object)
import OpenAPI.Types.ToEmailArray (mkTo_email_arrayItem)
import "sendgrid" OpenAPI.Common

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import GHC.Generics
import Data.Swagger hiding (Response, email)
import Control.Lens
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)
import BuildInfo (location)
import Control.Monad.IO.Class
import Network.HTTP.Client (responseStatus, responseBody)
import Network.HTTP.Types.Status (ok200, accepted202)
import Data.Functor (($>))
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Traversable (for)
import Data.Maybe (fromMaybe, isNothing)
import Control.Monad (when)

data Request = 
     Request 
     { from :: !Email
     , personalization :: !T.Text 
     , subject :: !T.Text
     , body :: !T.Text }
    deriving stock Generic
    deriving stock Show
    deriving (ToJSON, FromJSON)
       via WithOptions 
       '[ FieldLabelModifier '[ UserDefined (StripConstructor Request)]] 
       Request

instance ToSchema Request where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @T.Text)
    emailSchema <- declareSchemaRef (Proxy @Email)
    pure $ NamedSchema (Just ($location <> "." <> (show (typeRep @Request))^.stext)) $ mempty
         & type_ ?~ SwaggerObject
         & properties .~ fromList 
           [ ("personalization", textSchema)
           , ("from", emailSchema)
           , ("subject", textSchema)
           , ("body", textSchema) ]

controller :: Request -> KatipControllerM (Response ())
controller req@Request {..} = do
  $(logTM) InfoS $ logStr (show req)
  cfg <- fmap (^.katipEnv.sendGrid) ask
  resp <- for cfg $ \(SendGrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    let reqBody = 
          mkPOST_mail_sendRequestBody 
          [mkPOST_mail_sendRequestBodyContentsendgrid 
          "text/plain" 
          ("from:" <> 
            coerce from <> 
            ", " <> 
            personalization <> 
            ", message: " <> 
            body)]
          (mkFrom_email_object (coerce sendGridSenderIdentity))
          [((mkPOST_mail_sendRequestBodyPersonalizationssendgrid 
          (map (mkTo_email_arrayItem . coerce . email) sendGridPersons))
          {  pOST_mail_sendRequestBodyPersonalizationssendgridSend_at = Just tm
            , pOST_mail_sendRequestBodyPersonalizationssendgridSubject = Just $ subject })
          ]
          subject
    $(logTM) InfoS $ logStr (show (encodePretty reqBody))      
    resp <- liftIO $ runWithConfiguration sendgrid (pOST_mail_send (Just reqBody))
    let handleResp resp =
          if responseStatus resp == ok200 || 
            responseStatus resp == accepted202 
          then return $ Ok ()
          else $(logTM) ErrorS (logStr ("SendGrid error: " <> show (responseBody resp))) 
              $> Error (asError @T.Text "something went wrong")
    handleResp resp
  when (isNothing resp) $ $(logTM) InfoS "sendgrid key hasn't been found. skip"
  return $ fromMaybe (Ok ()) resp