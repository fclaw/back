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

module Scaffold.Api.Controller.SendGrid.SendMail (controller, Request) where

import Scaffold.Transport.Response
import Scaffold.Config (Email (..), SendGrid (..), Personalization (..))

import Katip
import KatipController
import Data.Aeson hiding (Error)
import Data.Aeson.Generic.DerivingVia
import GHC.Exts
import qualified Data.Text as T
import GHC.Generics
import Data.Swagger hiding (Response)
import Control.Lens
import Data.Proxy (Proxy (..))
import Type.Reflection (typeRep)
import Control.Lens.Iso.Extended (stext)
import BuildInfo (location)
import qualified Network.SendGridV3.Api as SendGrid 
import Data.Time.Clock.System (getSystemTime, systemSeconds)
import Control.Monad.IO.Class
import Network.HTTP.Client (HttpException (..))
import Data.Functor (($>))

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

controller :: Request -> KatipController (Response ())
controller req = do
  $(logTM) InfoS $ logStr (show req)
  SendGrid {..} <- fmap (^.katipEnv.sendGrid) ask
  let mail = mkMail req persons senderIdentity
  tm <- liftIO $ fmap systemSeconds getSystemTime
  let handleResp (Left ex@(InvalidUrlException _ _)) = 
         $(logTM) ErrorS (logStr (show ex)) $> Error (asError @T.Text "invalid url")
      handleResp (Left (HttpExceptionRequest _ ex)) = 
         $(logTM) ErrorS (logStr (show ex)) $> Error (asError @T.Text "error")
      handleResp _ = return $ Ok ()
  resp <- liftIO $ 
    SendGrid.sendMail 
    (SendGrid.ApiKey apiKey) 
    (mail { SendGrid._mailSendAt = Just (fromIntegral tm) })
  handleResp resp

mkMail :: Request -> [Personalization] -> Email -> SendGrid.Mail () ()
mkMail Request {..} xs sender =
  let to = SendGrid.personalization $ fromList $ xs <&> \Personalization {..} -> SendGrid.MailAddress (coerce email) personalization
      content = Just $ fromList [SendGrid.mailContentText body]
  in SendGrid.mail [to] (SendGrid.MailAddress (coerce sender) personalization) subject content
