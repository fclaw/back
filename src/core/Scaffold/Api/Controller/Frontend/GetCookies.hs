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

module Scaffold.Api.Controller.Frontend.GetCookies (controller, Cookie) where


import Scaffold.Transport.Response ( Response (..) )

import KatipController
import qualified Data.Text as T
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import GHC.Generics
import Data.Aeson
import Data.Text.Extended ()
import Control.Monad.IO.Class
import Data.Time.Clock (UTCTime, DiffTime, getCurrentTime, addUTCTime, secondsToNominalDiffTime)
import Data.Aeson.Generic.DerivingVia
import TH.Mk
import Data.List (stripPrefix)
import Data.Typeable (typeRep)
import Data.Proxy (Proxy (..))
import Data.Char (toLower)

-- | Data type representing the options for a <https://tools.ietf.org/html/draft-west-first-party-cookies-07#section-4.1 SameSite cookie>
data SameSiteOption 
    = Lax
    | Strict
    | None
    deriving stock Generic
    deriving (Show, Eq)

mkToSchemaAndJSON ''SameSiteOption

data Cookie = Cookie
    { cookieName :: !T.Text -- ^ The name of the cookie. Default value: @"name"@
    , cookieValue :: !T.Text -- ^ The value of the cookie. Default value: @"value"@
    , cookiePath :: !(Maybe T.Text) -- ^ The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
    , cookieExpires :: !(Maybe UTCTime) -- ^ The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
    , cookieMaxAge :: !(Maybe DiffTime) -- ^ The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
    , cookieDomain :: !(Maybe T.Text) -- ^ The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
    , cookieHttpOnly :: !Bool -- ^ Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
    , cookieSecure :: !Bool -- ^ Instructs the browser to only send the cookie over HTTPS. Default value: @False@
    , cookieSameSite :: !(Maybe SameSiteOption) -- ^ The "same site" policy of the cookie, i.e. whether it should be sent with cross-site requests. Default value: @Nothing@
    }
    deriving stock Generic
    deriving (ToJSON, FromJSON)
     via WithOptions 
     '[ FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Cookie)]] 
     Cookie

defaultSetCookie :: Cookie
defaultSetCookie = Cookie
    { cookieName     = "name"
    , cookieValue    = "value"
    , cookiePath     = Nothing
    , cookieExpires  = Nothing
    , cookieMaxAge   = Nothing
    , cookieDomain   = Nothing
    , cookieHttpOnly = False
    , cookieSecure   = False
    , cookieSameSite = Nothing
    }

deriveToSchemaFieldLabelModifier ''Cookie [| 
  \s -> let (head:tail) = show (typeRep (Proxy @Cookie))
        in maybe s (map toLower) (stripPrefix (toLower head : tail) s) |]

controller :: KatipControllerM (Response [Cookie])
controller = do
  tm <- liftIO getCurrentTime
  let expireTm = addUTCTime (secondsToNominalDiffTime 2592000) tm
  return $ Ok [defaultSetCookie { cookieName = "test", cookieExpires = Just expireTm }]