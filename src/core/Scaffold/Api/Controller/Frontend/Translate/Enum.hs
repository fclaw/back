{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Scaffold.Api.Controller.Frontend.Translate.Enum where

import Control.Lens
import Data.Default.Class
import GHC.Generics (Generic)
import TH.Mk

data Page = PageHome | PageAbout | PageService
  deriving stock (Generic)
  deriving (Enum)

instance Default Page where
  def = PageHome

data Menu = MenuHome | MenuAbout | MenuService
  deriving stock (Generic)
  deriving (Enum)

instance Default Menu where
  def = MenuHome

data Messenger = MessengerHeadline | MessengerIdentity | MessengerEmail | MessengerBody | MessengerButton
  deriving stock (Generic)

instance Default Messenger where
  def = MessengerHeadline

mkToSchemaAndJSON ''Page
mkEnumConvertor ''Page

mkToSchemaAndJSON ''Menu
mkEnumConvertor ''Menu

mkToSchemaAndJSON ''Messenger
mkEnumConvertor ''Messenger
