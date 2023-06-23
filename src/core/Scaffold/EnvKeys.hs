{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Scaffold.EnvKeys where

import Control.Lens
import Data.Aeson
import Data.Aeson.Generic.DerivingVia
import qualified Data.Map as M
import qualified Data.Text as T
import GHC.Generics

data Creds = Creds {repo :: !T.Text, key :: !T.Text, resources :: [T.Text]}
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined (StripConstructor Creds)]]
          Creds

newtype ReposCreds = ReposCreds (M.Map T.Text Creds)
  deriving stock (Generic)
  deriving stock (Show)
  deriving (FromJSON)

data EnvKeys = EnvKeys
  { envKeysSendgrid :: !(Maybe T.Text),
    envKeysTelegramBot :: !(Maybe T.Text),
    envKeysGithub :: !(Maybe ReposCreds),
    envKeysCaptchaKey :: !(Maybe T.Text)
  }
  deriving stock (Generic)
  deriving stock (Show)
  deriving
    (FromJSON)
    via WithOptions
          '[FieldLabelModifier '[UserDefined ToLower, UserDefined (StripConstructor EnvKeys)]]
          EnvKeys

makeFields ''Creds
makeFields ''EnvKeys
