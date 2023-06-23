{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Scaffold.Auth (User (..), withBasicAuth, checkBasicAuth) where

import Control.Monad.Except
import Crypto.JWT
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Katip
import Katip.Core
import KatipController (KatipControllerM, KatipLoggerLocIO)
import Scaffold.Transport.Response
import Servant (BasicAuthData (..))
import Servant.Auth.Server (AuthResult (..), BasicAuthCfg, FromBasicAuthData (..), FromJWT (decodeJWT), ToJWT (..), wwwAuthenticatedErr)

newtype User = User {email :: T.Text} deriving (Show)

instance FromJWT User where
  decodeJWT _ = undefined

instance ToJWT User where
  encodeJWT _ = emptyClaimsSet

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult User)

instance FromBasicAuthData User where
  fromBasicAuthData basicAuthData authChecker = authChecker basicAuthData

withBasicAuth :: AuthResult User -> (User -> KatipControllerM (Response a)) -> KatipControllerM (Response a)
withBasicAuth (Authenticated user) runApi = runApi user
withBasicAuth _ _ = throwError $ wwwAuthenticatedErr "only for authorized personnel"

checkBasicAuth :: KatipLoggerLocIO -> M.Map T.Text User -> BasicAuthData -> IO (AuthResult User)
checkBasicAuth log storage auth_data = do
  log getLoc InfoS $ ls $ show (basicAuthPassword auth_data, basicAuthUsername auth_data)
  return $ maybe Indefinite (const (Authenticated (User mempty))) $ T.decodeUtf8 (basicAuthPassword auth_data) `M.lookup` storage
