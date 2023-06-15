{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DerivingStrategies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}

module Scaffold.EnvKeys where

import Data.Aeson
import qualified Data.Text as T
import GHC.Generics
import Data.Aeson.Generic.DerivingVia
import Control.Lens
import qualified Data.Map as M

data Creds = Creds { repo :: !T.Text, key :: !T.Text, resources :: [T.Text] }
  deriving stock Generic
  deriving stock Show
  deriving FromJSON
    via WithOptions
    '[ FieldLabelModifier '[ UserDefined (StripConstructor Creds)]] 
    Creds

newtype ReposCreds = ReposCreds (M.Map T.Text Creds)
  deriving stock Generic
  deriving stock Show
  deriving FromJSON

data EnvKeys = 
     EnvKeys 
     { envKeysSendgrid :: !(Maybe T.Text)
     , envKeysTelegramBot :: !(Maybe T.Text)
     , envKeysGithub :: !(Maybe ReposCreds) }
  deriving stock Generic
  deriving stock Show
  deriving FromJSON
    via WithOptions
    '[ FieldLabelModifier '[ UserDefined (StripConstructor EnvKeys)]] 
    EnvKeys

makeFields ''Creds
makeFields ''EnvKeys