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

module Scaffold.Api.Controller.Frontend.GetCookies (controller, cookieTitle, Cookie) where

import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Proxy (Proxy (..))
import Data.Swagger.Schema.Extended (deriveToSchemaFieldLabelModifier)
import qualified Data.Text as T
import Data.Text.Extended ()
import Data.Time.Clock (DiffTime, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Typeable (typeRep)
import GHC.Generics
import KatipController
import Scaffold.Transport.Response (Response (..))
import System.Random (getStdGen, randomR)
import TH.Mk

cookieTitle :: T.Text
cookieTitle = "_tth"

-- | Data type representing the options for a <https://tools.ietf.org/html/draft-west-first-party-cookies-07#section-4.1 SameSite cookie>
data SameSiteOption
  = Lax
  | Strict
  | None
  deriving stock (Generic)
  deriving (Show, Eq)

mkToSchemaAndJSON ''SameSiteOption

data Cookie = Cookie
  { -- | The name of the cookie. Default value: @"name"@
    cookieName :: !T.Text,
    -- | The value of the cookie. Default value: @"value"@
    cookieValue :: !T.Text,
    -- | The URL path for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the path of the request that sets the cookie).
    cookiePath :: !(Maybe T.Text),
    -- | The time at which to expire the cookie. Default value: @Nothing@ (The browser will default to expiring a cookie when the browser is closed).
    cookieExpires :: !(Maybe UTCTime),
    -- | The maximum time to keep the cookie, in seconds. Default value: @Nothing@ (The browser defaults to expiring a cookie when the browser is closed).
    cookieMaxAge :: !(Maybe DiffTime),
    -- | The domain for which the cookie should be sent. Default value: @Nothing@ (The browser defaults to the current domain).
    cookieDomain :: !(Maybe T.Text),
    -- | Marks the cookie as "HTTP only", i.e. not accessible from Javascript. Default value: @False@
    cookieHttpOnly :: !Bool,
    -- | Instructs the browser to only send the cookie over HTTPS. Default value: @False@
    cookieSecure :: !Bool,
    -- | The "same site" policy of the cookie, i.e. whether it should be sent with cross-site requests. Default value: @Nothing@
    cookieSameSite :: !(Maybe SameSiteOption)
  }
  deriving stock (Generic)
  deriving
    (ToJSON, FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor Cookie)]]
          Cookie

defaultSetCookie :: Cookie
defaultSetCookie =
  Cookie
    { cookieName = "name",
      cookieValue = "value",
      cookiePath = Nothing,
      cookieExpires = Nothing,
      cookieMaxAge = Nothing,
      cookieDomain = Nothing,
      cookieHttpOnly = False,
      cookieSecure = False,
      cookieSameSite = Nothing
    }

deriveToSchemaFieldLabelModifier
  ''Cookie
  [|
    \s ->
      let (head : tail) = show (typeRep (Proxy @Cookie))
       in maybe s (map toLower) (stripPrefix (toLower head : tail) s)
    |]

controller :: KatipControllerM (Response [Cookie])
controller = do
  tm <- liftIO getCurrentTime
  ident <- fmap (T.pack . show . fst . randomR @Int (10000000, 50000000)) $ liftIO getStdGen
  let expireTm = addUTCTime (secondsToNominalDiffTime 2592000) tm
  return $ Ok [defaultSetCookie {cookieName = cookieTitle, cookieExpires = Just expireTm, cookieValue = ident}]
