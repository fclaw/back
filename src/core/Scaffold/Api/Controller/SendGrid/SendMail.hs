{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.SendGrid.SendMail (controller, SendGridSendMailRequest) where

import Control.Lens
import Control.Monad (when)
import Control.Monad.IO.Class
import Data.Aeson hiding (Error)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.Functor (($>))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe, isNothing)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Data.Traversable (for)
import Data.Typeable (typeRep)
import GHC.Exts
import GHC.Generics
import Katip
import KatipController
import Network.HTTP.Client (responseBody, responseStatus)
import Network.HTTP.Types.Status (accepted202, ok200)
import "sendgrid" OpenAPI.Common
import OpenAPI.Operations.POST_mail_send
  ( mkPOST_mail_sendRequestBody,
    mkPOST_mail_sendRequestBodyContentsendgrid,
    mkPOST_mail_sendRequestBodyPersonalizationssendgrid,
    pOST_mail_send,
    pOST_mail_sendRequestBodyPersonalizationssendgridSend_at,
    pOST_mail_sendRequestBodyPersonalizationssendgridSubject,
  )
import OpenAPI.Types.FromEmailObject (mkFrom_email_object)
import OpenAPI.Types.ToEmailArray (mkTo_email_arrayItem)
import Scaffold.Config (Email (..), SendGrid (..), personalizationEmail)
import Scaffold.Transport.Response

data SendGridSendMailRequest = SendGridSendMailRequest
  { from :: !Email,
    personalization :: !T.Text,
    subject :: !T.Text,
    body :: !T.Text
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor SendGridSendMailRequest)]]
          SendGridSendMailRequest

deriveToSchemaFieldLabelModifier
  ''SendGridSendMailRequest
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @SendGridSendMailRequest))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: SendGridSendMailRequest -> KatipControllerM (Response ())
controller req@SendGridSendMailRequest {..} = do
  $(logTM) InfoS $ logStr (show req)
  cfg <- fmap (^. katipEnv . sendGrid) ask
  resp <- for cfg $ \(SendGrid {..}, sendgrid) -> do
    tm <- fmap (fromIntegral . systemSeconds) $ liftIO $ getSystemTime
    let reqBody =
          mkPOST_mail_sendRequestBody
            [ mkPOST_mail_sendRequestBodyContentsendgrid
                "text/plain"
                ( "from:"
                    <> coerce from
                    <> ", "
                    <> personalization
                    <> ", message: "
                    <> body
                )
            ]
            (mkFrom_email_object (coerce sendGridSenderIdentity))
            [ ( ( mkPOST_mail_sendRequestBodyPersonalizationssendgrid
                    (map (mkTo_email_arrayItem . coerce . personalizationEmail) sendGridPersons)
                )
                  { pOST_mail_sendRequestBodyPersonalizationssendgridSend_at = Just tm,
                    pOST_mail_sendRequestBodyPersonalizationssendgridSubject = Just $ subject
                  }
              )
            ]
            subject
    $(logTM) InfoS $ logStr (show (encodePretty reqBody))
    resp <- liftIO $ runWithConfiguration sendgrid (pOST_mail_send (Just reqBody))
    let handleResp resp =
          if responseStatus resp == ok200
            || responseStatus resp == accepted202
            then return $ Ok ()
            else
              $(logTM) ErrorS (logStr ("SendGrid error: " <> show (responseBody resp)))
                $> Error (asError @T.Text "something went wrong")
    handleResp resp
  when (isNothing resp) $ $(logTM) InfoS "sendgrid key hasn't been found. skip"
  return $ fromMaybe (Ok ()) resp
